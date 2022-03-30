(import ./new_gap_buffer :prefix "")
(import ./state)
(import spork/path)

(defn string->path-line-column
  ``
  Takes a string with format "path:line:column"
  and turns it into [path line column].
  ``
  [path-string]
  (let [[path line column]
        (peg/match
          '(* (<- (some (if-not ":" 1)))
              (opt (* ":"
                      (<- (some (if-not ":" 1)))))
              (opt (* ":"
                      (<- (some (if-not ":" 1))))))
          path-string)]
    [path
     (-?> line scan-number)
     (-?> column scan-number)]))

(comment
  (string->path-line-column
    "aoeu.exe")
  #=> ["aoeu.exe" nil nil]

  (string->path-line-column
    "aoeu.exe:123")
  #=> ["aoeu.exe" 123 nil]

  (string->path-line-column
    "aoeu.exe:123:5")
  #=> ["aoeu.exe" 123 5]
  #
)


#(setdyn :freja/ns "freja/file-handling")

(defn ensure-dir
  [path]
  (reduce (fn [acc cur]
            (if-not acc
              cur
              (let [new (string acc path/sep cur)]
                (os/mkdir new)
                new)))
          nil
          (string/split path/sep path)))

(defn data-path
  [&opt path]
  (if (= :windows (os/which))
    (string (os/getenv "LOCALAPPDATA") path/sep "freja" (when path (string path/sep path)))
    (string (os/getenv "HOME") "/.local/share/freja" (when path (string path/sep path)))))

(var scratch-path nil)

(defn remove-scratch-file
  []
  (os/rm scratch-path))

(varfn read-file
  [path]
  (def f (file/open path))
  (if (not f) #(error (string "File " path " not found."))
    (do (print "File " path " not found -- saving will create a new file.")
      "")
    (with [f f]
      (def text (file/read f :all))
      text)))

(varfn load-file
  [props path]
  (-> (props :gb)
      (replace-content (read-file path))
      (put :path path)
      (put :caret 0)
      (put :scroll 0)
      (put :selection nil))

  (put props :event/changed true))

(varfn save-file
  [props &keys {:no-print no-print}]
  (def path (props :path))

  (with [f (file/open path :w)]
    (file/write f (-> props
                      commit!
                      (get :text)))
    (file/flush f)
    (unless no-print
      (print "Saved file: " path))))

(comment
  (import freja/frp)
  (frp/subscribe! frp/mouse |(lul $))

  (replace-content gb-data "")
  (let [_ (load-file gb-data "freja/test_main.janet")]
    :ok)

  (let [_ (load-file2 text-data "freja/main.janet")]
    :ok)

  (let [_ (load-file text-data "freja/text_rendering_ints.janet")]
    :ok)

  (put text-data :text (buffer (text-data :after))))

(var last-path nil)

(defn freja-dofile*
  `Evaluate a file and return the resulting environment. :env, :expander,
  :evaluator, :read, and :parser are passed through to the underlying
  run-context call. If exit is true, any top level errors will trigger a
  call to (os/exit 1) after printing the error.`
  [path &keys
   {:exit exit
    :env env
    :source src
    :expander expander
    :evaluator evaluator
    :read read
    :parser parser}]
  (def f (if (= (type path) :core/file)
           path
           (file/open path :rb)))
  (def path-is-file (= f path))
  (default env (make-env))
  (def spath (string path))
  (put env :current-file (or src (if-not path-is-file spath)))
  (put env :source (or src (if-not path-is-file spath path)))
  (var exit-error nil)
  (var exit-fiber nil)
  (defn chunks [buf _] (file/read f 4096 buf))
  (defn bp [&opt x y]
    (when exit
      (bad-parse x y)
      (os/exit 1))
    (put env :exit true)
    (def buf @"")
    (with-dyns [:err buf :err-color false]
      (bad-parse x y))
    (set exit-error (string/slice buf 0 -2)))
  (defn bc [&opt x y file-name a b]
    (when exit
      (bad-compile x y file-name a b)
      (os/exit 1))
    (put env :exit true)
    (def buf @"")
    (with-dyns [:err buf :err-color false]
      (bad-compile x nil file-name a b))
    (set exit-error {:source file-name
                     :source-line a
                     :error-msg (string/slice buf 0 -2)})
    (set exit-fiber y))
  (unless f
    (error (string "could not find file " path)))
  (def nenv
    (run-context {:env env
                  :chunks chunks
                  :on-parse-error bp
                  :on-compile-error bc
                  :on-status (fn [f x]
                               (when (not= (fiber/status f) :dead)
                                 (when exit
                                   (eprint x)
                                   (print "run-context stacktrace thing:")
                                   (debug/stacktrace f)
                                   (eflush)
                                   (os/exit 1))
                                 (put env :exit true)
                                 (set exit-error x)
                                 (set exit-fiber f)))
                  :evaluator evaluator
                  :expander expander
                  :read read
                  :parser parser
                  :source (or src (if path-is-file "<anonymous>" spath))}))
  (env :exit)
  (if-not path-is-file (file/close f))
  (when exit-error
    (if exit-fiber
      (propagate exit-error exit-fiber)
      (error exit-error)))
  nenv)

(comment
  (freja-dofile* "dumb.janet")
  #
)

(defn- no-side-effects
  `Check if form may have side effects. If returns true, then the src
  must not have side effects, such as calling a C function.`
  [src]
  (cond
    (tuple? src)
    (if (= (tuple/type src) :brackets)
      (all no-side-effects src))
    (array? src)
    (all no-side-effects src)
    (dictionary? src)
    (and (all no-side-effects (keys src))
         (all no-side-effects (values src)))
    true))
(defn- is-safe-def [x] (no-side-effects (last x)))
(def- safe-forms {'defn true 'varfn true 'defn- true 'defmacro true 'defmacro- true
                  'def is-safe-def 'var is-safe-def 'def- is-safe-def 'var- is-safe-def
                  'defglobal is-safe-def 'varglobal is-safe-def})
(def- importers {'import true 'import* true 'dofile true 'require true})

(defn- use-2 [evaluator args]
  (each a args (import* (string a) :prefix "" :evaluator evaluator)))

(defn- flycheck-evaluator
  ``An evaluator function that is passed to `run-context` that lints (flychecks) code.
  This means code will parsed and compiled, macros executed, but the code will not be run.
  Used by `flycheck`.``
  [thunk source env where]
  (when (tuple? source)
    (def head (source 0))
    (def safe-check
      (or
        (safe-forms head)
        (if (symbol? head)
          (if (string/has-prefix? "define-" head) is-safe-def))))
    (cond
      # Sometimes safe form
      (function? safe-check)
      (if (safe-check source) (thunk))
      # Always safe form
      safe-check
      (thunk)

      # Use
      (= 'use head)
      (thunk)
      # for some reason, doing this can break :export inside modules
      # so I disabled it
      #(use-2 flycheck-evaluator (tuple/slice source 1))


      # Import-like form
      (importers head)
      (thunk)
      # for some reason, doing this can break :export inside modules
      # so I disabled it
      #(let [[l c] (tuple/sourcemap source)
      #      newtup (tuple/setmap (tuple ;source :evaluator flycheck-evaluator) l c)]
      #  ((compile newtup env where)))
)))

(defn freja-dofile
  [top-env path]
  (print (string `=> (freja-dofile "` path `")`))

  (def module-path
    (if (path/ext path)
      (string/slice path 0 (-> (path/ext path) length - dec))
      path))

  (def ns-path (or (first (module/find (path/abspath module-path)))
                   (first (module/find module-path))
                   path))

  (def env
    #(get module/cache path)
    (when ns-path
      (module/cache ns-path)))

  (default env (make-env))

  (put env :redef true)
  (put env :out state/out)
  (put env :err state/out)
  (put env :freja/loading-file true)

  (def env (dofile path :env env))

  (put module/cache ns-path env)

  (set state/user-env env)

  (comment
    (def path (if (path/abspath? path)
                path
                (string "./" path)))

    (unless (= path last-path)
      (set state/user-env (make-env top-env))
      (set last-path path))

    (with-dyns [:out state/out
                :err state/out]
      (print (string `=> (freja-dofile "` path `")`))

      (def module-path
        (if (path/ext path)
          (string/slice path 0 (-> (path/ext path) length - dec))
          path))

      (def ns-path (or (first (module/find (path/abspath module-path)))
                       (first (module/find module-path))))

      (def existing-env (when ns-path (module/cache ns-path)))

      (def defonced-symbols
        (if-not existing-env
          []
          (seq [[k v] :pairs existing-env
                :when (and (table? v)
                           (v :defonce))]
            [k v])))

      (def env (or # existing-env
                   (make-env top-env)))

      (put env :freja/loading-file true)
      (put env :out state/out)
      (put env :err state/out)

      (def before-keys (keys env))

      # this is done to make defonce work
      # the only problem is that old symbols might come along for the ride
      # flychecking will stop people from using old symbols
      # but they will still be kept in the environment
      (loop [[k v] :in defonced-symbols]
        (put env k v))

      (try
        (do
          # flychecking
          (freja-dofile* path :evaluator flycheck-evaluator)

          (freja-dofile* path :env env))
        ([err fib]
          (if (dictionary? err)
            (let [{:error-msg msg
                   :source source
                   :source-line source-line
                   :code code} err]
              (comment
                (e/push! state/eval-results {:error msg
                                             :source source
                                             :source-line source-line
                                             :fiber fib

                                             :code (or code (string `(freja-dofile "` path `")`))
                                             :cause "freja-dofile"}))
              (propagate {:error msg
                          :source source
                          :source-line source-line
                          :fiber fib

                          #:code (or code (string `(freja-dofile "` path `")`))
                          :cause "freja-dofile"}
                         fib)
              #            (propagate err fib)
)
            (do
              #            (e/push! state/eval-results {:error err
              #                                       :code (string `(freja-dofile "` path `")`)
              #                                       :fiber fib
              #                                       :cause "freja-dofile"})
              (propagate err fib)))))

      # here we find all `var` / `varfn` defined during `dofile`
      (def new-vars (seq [k :keys env
                          :when (and (not (find |(= $ k) before-keys))
                                     (get-in env [k :ref]))]
                      k))

      (def ns-name (get env :freja/ns))

      (def mod-name (or (first (module/find (path/abspath module-path)))
                        (first (module/find module-path))))

      (set state/user-env env)

      # TODO: fix this and ensure it works with any .janet-file afterwards
      (cond existing-env
        (let [ns existing-env]
          (loop [k :keys env
                 :let [existing-sym (ns k)
                       existing-var (get-in ns [k :ref])
                       new-var (get-in env [k :ref])]]
            (cond
              (or (not existing-sym)
                  (and (not existing-var)
                       (not new-var)))
              (put ns k (in env k))

              (and
                # doesn't make much sense to keep track of private vars, since they're local to a file (hopefully)
                (not (existing-sym :private))
                existing-var new-var)
              (do
                (print "new var " k " in ns " ns-name " replaces var " (string/format "%P" existing-sym))

                # we want to keep existing var and update it
                # since this is what existing functions will have as reference
                (put-in ns [k :ref] existing-var)
                (put existing-var 0 (in new-var 0)))

              new-var
              (put ns k (in env k))

              # else only existing-var
              (put ns k (in env k))))

          (set state/user-env ns))

        ns-name
        (do
          (put module/cache path env)
          (set state/user-env env))

        #else
        (do
          (print "Module " path " did not exist, adding to module/cache...")
          (put module/cache path env)
          (set state/user-env env)))

      (e/push! state/eval-results {:value (string "Loaded module: " (or ns-name path))
                                   #:code (string `(freja-dofile "` path `")`)
                                   :fiber (fiber/current)})))
  #
)


(comment
  (freja-dofile (curenv) "test.janet")

  (put-in module/cache ["freja/file-handling" 'fine] @{:ref @[fine]})

  #  (freja-dofile state/user-env "test-env.janet")

  (comment
    (freja-dofile state/user-env "test-env2.janet")
    #
)
  #  (lul2)
  #

  (import freja/file-handling)
  (file-handling/fine)
  (curenv)
  (state/user-env))

(varfn save-and-dofile
  [props]
  (def path (props :path))
  (save-file props :no-print true)
  (freja-dofile (get-in props [:context :top-env]) path))

(varfn df
  [path]
  (do (freja-dofile (fiber/getenv (fiber/current)) path) :ok))


(comment

  (df "misc/frp3.janet")

  (dofile "/Users/test/programmering/janet/textfield/freja/text_rendering_ints.janet"
          :env (fiber/getenv (fiber/current)))

  (dofile "/Users/test/programmering/janet/textfield/freja/new_gap_buffer.janet"
          :env (fiber/getenv (fiber/current)))

  (dofile "/Users/test/programmering/janet/textfield/freja/main.janet"
          {:env (get-in text-data [:context :top-env])}))
