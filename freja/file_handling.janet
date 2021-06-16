(import ./new_gap_buffer :prefix "")
(import ./text_api :as old)

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
  (-> props
      (replace-content (read-file path))
      (put :path path)
      (put :caret 0)
      (put :scroll 0)
      (put :selection nil)))

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
  [props]
  (def path (props :path))

  (with [f (file/open path :w)]
    (file/write f (-> props
                      commit!
                      (get :text)))
    (file/flush f)
    (print "Saved file: " path)))

(varfn inner-dofile
  [top-env path]
  
  (def env (make-env top-env))
  
  (try
    (do
      (dofile path
              #             :env (fiber/getenv (fiber/current))
              :env env)
      (merge-into top-env env))
    ([err fib]
      (print "nope")
      (print (debug/stacktrace fib err)))))

(varfn save-and-dofile
  [props]
  (def path (props :path))
  (save-file props)
  
  (inner-dofile (get-in props [:context :top-env]) path)
  
  (print "Loaded file: " path))

(varfn df
  [path]
  (do (inner-dofile (fiber/getenv (fiber/current)) path) :ok))


(comment

  (df "misc/frp3.janet")
  

  (dofile "/Users/test/programmering/janet/textfield/freja/text_rendering_ints.janet"
          :env (fiber/getenv (fiber/current)))

  (dofile "/Users/test/programmering/janet/textfield/freja/new_gap_buffer.janet"
          :env (fiber/getenv (fiber/current)))

  (dofile "/Users/test/programmering/janet/textfield/freja/main.janet"
          {:env (get-in text-data [:context :top-env])}))
