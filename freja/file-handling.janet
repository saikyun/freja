(import ./new_gap_buffer :prefix "")
(import ./text_api :as old)
(import freja/state)

(setdyn :freja/ns "freja/file-handling")

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

(varfn load-file2
  [props path]
  (old/replace-content props (read-file path)))

(comment
  (replace-content gb-data "")
  (let [_ (load-file gb-data "freja/test_main.janet")]
    :ok)

  (let [_ (load-file2 text-data "freja/main.janet")]
    :ok)

  (let [_ (load-file text-data "freja/text_rendering_ints.janet")]
    :ok)

  (put text-data :text (buffer (text-data :after))))

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

(var last-path nil)

(varfn freja-dofile
  [top-env path]
  (unless (= path last-path)
    (set state/user-env (make-env top-env))
    (set last-path path))

  (try
    (with-dyns [:out state/out
                :err state/out]

      (def env (make-env top-env))

      (print `=> (freja-dofile "` path `")`)

      (put env :freja/loading-file true)
      (put env :out state/out)
      (put env :err state/out)

      (def before-keys (keys env))

      (dofile path
              # :env (fiber/getenv (fiber/current))
              #:env
              #(require "freja/render_new_gap_buffer")
              :env env)

      # here we find all `var` / `varfn` defined during `dofile`
      (def new-vars (seq [k :keys env
                          :when (and (not (find |(= $ k) before-keys))
                                     (get-in env [k :ref]))]
                      k))

      (def ns-name (get env :freja/ns))
      (def modpath (first (module/find path)))
      (cond ns-name
        (let [ns (require ns-name)]
          (loop [k :in new-vars]
            (if (ns k)
              (put-in ns [k :ref 0] (get-in env [k :ref 0]))
              (put ns k (in env k))))

          (set state/user-env ns))

        modpath
        (let [ns (require modpath)]
          (loop [k :in new-vars]
            (put-in ns [k :ref 0] (get-in env [k :ref 0])))

          (set state/user-env ns))

        #else
        (let [ns env]
          (print "Module " path " did not exist, adding to module/cache...")

          (put module/cache path ns)

          (set state/user-env ns)))

      (print "Loaded module: " (or (get env :freja/ns) modpath path)))
    ([err fib]
      (with-dyns [:out state/out
                  :err state/err]
        (debug/stacktrace fib err)))))

(comment
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

(varfn fine
  "HHUUUH"
  []
  (print "fine123"))

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
