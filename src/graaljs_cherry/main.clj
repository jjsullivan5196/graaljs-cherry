(ns graaljs-cherry.main
  "A REPL that compiles cherry expressions on the JVM and evaluates the
  resulting JS in an embedded GraalJS context."
  (:require
   [cherry.compiler :as compiler]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (clojure.lang LineNumberingPushbackReader)
   (java.net URI)
   (org.graalvm.polyglot Context PolyglotException Source Value)
   (org.graalvm.polyglot.io FileSystem IOAccess)
   (org.graalvm.polyglot.proxy ProxyExecutable))
  (:gen-class))

(set! *warn-on-reflection* true)

;;;; Module resolution

;; Cherry-compiled code imports bare specifiers like 'cherry-cljs/cljs.core.js'.
;; GraalJS resolves import specifiers through the polyglot FileSystem, so remap
;; bare specifiers to node_modules; everything else delegates to the default fs.

(defn- node-modules-dir ^String []
  (or (System/getenv "CHERRY_NODE_MODULES")
      (str (System/getProperty "user.dir") "/node_modules")))

(defn- bare-specifier? [^String path]
  (not (or (str/starts-with? path "/")
           (str/starts-with? path "./")
           (str/starts-with? path "../"))))

(defn- exports-target
  "Resolve a package.json exports value to a package-relative file path."
  [exports subpath]
  (let [subkey (if subpath (str "./" subpath) ".")
        node (if (and (map? exports)
                      (some #(str/starts-with? % ".") (keys exports)))
               (get exports subkey)
               (when-not subpath exports))]
    (loop [node node]
      (cond
        (string? node) node
        (map? node) (recur (or (get node "import")
                               (get node "module")
                               (get node "node")
                               (get node "default")))
        :else nil))))

(defn- resolve-specifier
  "Node-ish resolution of a bare import specifier against node_modules:
  literal file, then package.json exports/module/main, then index.js.
  ESM packages only."
  ^String [^String spec]
  (let [nm (node-modules-dir)
        direct (io/file nm spec)]
    (if (.isFile direct)
      (.getPath direct)
      (let [segs (str/split spec #"/")
            npkg (if (str/starts-with? spec "@") 2 1)
            pkg-dir (io/file nm (str/join "/" (take npkg segs)))
            subpath (not-empty (str/join "/" (drop npkg segs)))
            pj-file (io/file pkg-dir "package.json")
            pj (when (.isFile pj-file)
                 (try (json/read-str (slurp pj-file))
                      (catch Exception _ nil)))
            entry (or (exports-target (get pj "exports") subpath)
                      (when subpath
                        (cond
                          (.isFile (io/file pkg-dir (str subpath ".js"))) (str subpath ".js")
                          (.isDirectory (io/file pkg-dir subpath)) (str subpath "/index.js")))
                      (when-not subpath
                        (or (get pj "module") (get pj "main"))))]
        (.getPath (io/file pkg-dir (or entry "index.js")))))))

(defn- module-fs ^FileSystem []
  (let [delegate (FileSystem/newDefaultFileSystem)]
    (reify FileSystem
      (^java.nio.file.Path parsePath [_ ^String path]
        (.parsePath delegate
                    (if (bare-specifier? path)
                      (resolve-specifier path)
                      path)))
      (^java.nio.file.Path parsePath [_ ^URI uri]
        (.parsePath delegate uri))
      (checkAccess [_ path modes link-options]
        (.checkAccess delegate path modes link-options))
      (createDirectory [_ dir attrs]
        (.createDirectory delegate dir attrs))
      (delete [_ path]
        (.delete delegate path))
      (newByteChannel [_ path options attrs]
        (.newByteChannel delegate path options attrs))
      (newDirectoryStream [_ dir filter]
        (.newDirectoryStream delegate dir filter))
      (toAbsolutePath [_ path]
        (.toAbsolutePath delegate path))
      (toRealPath [_ path link-options]
        (.toRealPath delegate path link-options))
      (readAttributes [_ path attributes options]
        (.readAttributes delegate path attributes options)))))

;;;; GraalJS context

(defn make-context ^Context []
  (-> (Context/newBuilder ^"[Ljava.lang.String;" (into-array String ["js"]))
      (.allowIO (-> (IOAccess/newBuilder)
                    (.fileSystem (module-fs))
                    (.build)))
      (.allowExperimentalOptions true)
      (.option "engine.WarnInterpreterOnly" "false")
      (.build)))

(defn- callback ^ProxyExecutable [f]
  (reify ProxyExecutable
    (execute [_ args]
      (f (aget args 0))
      nil)))

(defn- error->str [^Context ctx ^Value err]
  (-> (.eval ctx "js" "(e) => (e instanceof Error && e.stack) ? e.stack : String(e)")
      (.execute (object-array [err]))
      (.asString)))

(defn eval-await
  "Eval `js-code` (which must evaluate to a promise) and wait for it to settle."
  ^Value [^Context ctx ^String js-code]
  (let [src (.buildLiteral (Source/newBuilder "js" js-code "<repl>"))
        p (.eval ctx src)
        result (promise)]
    (.invokeMember p "then"
                   (object-array [(callback #(deliver result [:ok %]))
                                  (callback #(deliver result [:err %]))]))
    ;; promise jobs run when a polyglot call returns to the host; module loading
    ;; is synchronous through the fs, so pumping is only a safety net
    (loop [i 0]
      (when (and (not (realized? result)) (< i 1000))
        (.eval ctx "js" "undefined")
        (recur (inc i))))
    (if (realized? result)
      (let [[status ^Value v] @result]
        (if (= :ok status)
          (.getArrayElement v 0)
          (throw (Exception. ^String (str (if (instance? Value v)
                                            (error->str ctx v)
                                            (str v)))))))
      (throw (Exception. "timed out waiting for promise")))))

(def ^:private bootstrap-js
  "(async function () {
     globalThis.cherry_repl_core = await import('cherry-cljs/cljs.core.js');
     // graaljs console stringifies with ToString, which throws on
     // null-prototype objects (e.g. module namespaces); fall back to the
     // object's tag and keys
     const safe = (x) => {
       try { String(x); return x; }
       catch (_e) {
         const tag = Object.prototype.toString.call(x);
         try {
           const keys = Object.keys(x);
           const shown = keys.slice(0, 20).join(', ');
           return tag + ' {' + shown + (keys.length > 20 ? ', ...' : '') + '}';
         } catch (_e2) { return tag; }
       }
     };
     for (const m of ['log', 'info', 'warn', 'error', 'debug']) {
       const orig = console[m];
       if (orig) console[m] = (...args) => orig.apply(console, args.map(safe));
     }
     return [undefined];
   })()")

(defn bootstrap
  "Load cljs.core into the context; returns its module namespace Value."
  ^Value [^Context ctx]
  (eval-await ctx bootstrap-js)
  (.getMember (.getBindings ctx "js") "cherry_repl_core"))

;;;; Compilation

(defn compile-form
  "Compile one repl input. The compiled snippet is an async IIFE
  resolving to the value in a one-element box."
  [form state]
  (let [form (if-not (string? form) form (pr-str form))
        opts {:context       :repl-return
              :repl          true
              :async         true
              :elide-exports true}
        {:keys [javascript] :as new-state}
        (compiler/compile-form* form (merge (or state {}) opts))]
    (merge new-state
           {:javascript (str "(async function () {\n"
                             javascript
                             "\n;return [undefined];\n})()")})))

;;;; REPL

;;;;;;; from clojure.main


(defn skip-if-eol
  "If the next character on stream s is a newline, skips it, otherwise
  leaves the stream untouched. Returns :line-start, :stream-end, or :body
  to indicate the relative location of the next character on s. The stream
  must either be an instance of LineNumberingPushbackReader or duplicate
  its behavior of both supporting .unread and collapsing all of CR, LF, and
  CRLF to a single \\newline."
  [^LineNumberingPushbackReader s]
  (let [c (.read s)]
    (cond
      (= c (int \newline)) :line-start
      (= c -1) :stream-end
      :else (do (.unread s c) :body))))

(defn skip-whitespace
  "Skips whitespace characters on stream s. Returns :line-start, :stream-end,
  or :body to indicate the relative location of the next character on s.
  Interprets comma as whitespace and semicolon as comment to end of line.
  Does not interpret #! as comment to end of line because only one
  character of lookahead is available. The stream must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF to a single
  \\newline."
  [^LineNumberingPushbackReader s]
  (loop [c (.read s)]
    (cond
     (= c (int \newline)) :line-start
     (= c -1) :stream-end
     (= c (int \;)) (do (.readLine s) :line-start)
     (or (Character/isWhitespace (char c)) (= c (int \,))) (recur (.read s))
     :else (do (.unread s c) :body))))

(defn cljs-repl-read
  [request-prompt request-exit]
  (or ({:line-start request-prompt :stream-end request-exit}
       (skip-whitespace *in*))
      (let [input (read {:read-cond :allow :features #{:cljs}} *in*)]
        (skip-if-eol *in*)
        input)))

(defn repl [^Context ctx ^Value pr-str*]
  (let [request-prompt (Object.)
        request-exit   (Object.)
        repl-state     (atom nil)
        prompt         #(printf "%s=> " (:ns @repl-state "user"))]
    (loop []
      (prompt)
      (flush)
      (let [input (try (cljs-repl-read request-prompt request-exit)
                       (catch Exception e
                         (binding [*out* *err*]
                           (println (ex-message e)))
                         request-prompt))]
        (when-not (= request-exit input)
          (when-not (= request-prompt input)
            (try
              (let [{:keys [javascript] :as _new-state}
                    (swap! repl-state #(compile-form input %))]
                (when (System/getenv "CHERRY_PRINT_JS")
                  (println javascript))
                (println (.asString (.execute pr-str* (object-array [(eval-await ctx javascript)])))))
              (catch Exception e
                (binding [*out* *err*]
                  (println (ex-message e))))))
          (recur))))))

(defn eval-once
  "Eval one expression; prn the result when non-nil. Returns an exit code."
  [^Context ctx ^Value pr-str* ^String code]
  (let [form (read-string {:read-cond :allow :features #{:cljs}} code)
        {:keys [javascript] :as _}
        (try (compile-form form nil)
             (catch Exception e
               (binding [*out* *err*]
                 (println "compile error:" (ex-message e)))
               nil))]
    (if-not javascript
      1
      (try
        (let [v (eval-await ctx form)]
          (when-not (.isNull v)
            (println (.asString (.execute pr-str* (object-array [v])))))
          0)
        (catch Exception e
          (binding [*out* *err*]
            (println (ex-message e))
            1))))))

(defn -main [& args]
  (with-open [ctx (make-context)]
    (let [core (bootstrap ctx)
          pr-str* (.getMember core "pr_str")]
      (if (= "-e" (first args))
        (if-let [code (second args)]
          (System/exit (eval-once ctx pr-str* code))
          (binding [*out* *err*]
            (println "usage: graaljs-cherry [-e expr]")
            (System/exit 1)))
        (do (println "Cherry GraalJS REPL, Ctrl-D to exit")
            (repl ctx pr-str*)))))
  (System/exit 0))
