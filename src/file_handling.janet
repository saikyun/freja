(import ./text_api :prefix "")

(varfn content
  "Returns a big string of all the pieces in the text data."
  [{:selected selected :text text :after after}]
  (string text selected (string/reverse after)))

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

(varfn save-file
  [props path]
  (with [f (file/open path :w)]
        (file/write f (content props))
        (file/flush f)))
