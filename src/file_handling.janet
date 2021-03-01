(import ./new_gap_buffer :prefix "")
(import ./text_api :as old)

(varfn read-file
  [path]
  (def f (file/open path))
  (when (not f) (error (string "File " path " not found.")))
  (with [f f]
    (def text (file/read f :all))
    text))

(varfn load-file
  [props path]
  (replace-content props (read-file path)))

(varfn load-file2
  [props path]
  (old/replace-content props (read-file path)))

(comment
  (replace-content gb-data "")
  (let [_ (load-file gb-data "src/main.janet")]
    :ok)
  
  (let [_ (load-file2 text-data "src/main.janet")]
    :ok)
  
  (let [_ (load-file text-data "src/text_rendering_ints.janet")]
    :ok)
  
  (put text-data :text (buffer (text-data :after)))
  )

(varfn save-file
  [props path]
  (with [f (file/open path :w)]
    (file/write f (-> props
                      commit!
                      (get :text)))
    (file/flush f)))


(comment
  (dofile "/Users/test/programmering/janet/textfield/src/text_rendering_ints.janet"
          :env (fiber/getenv (fiber/current)))
  
  (dofile "/Users/test/programmering/janet/textfield/src/new_gap_buffer.janet"
          :env (fiber/getenv (fiber/current)))
  
  (dofile "/Users/test/programmering/janet/textfield/src/main.janet"
          {:env (get-in text-data [:context :top-env])})
  )
