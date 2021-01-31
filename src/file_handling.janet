(import ./text_api :prefix "")

(varfn content
  "Returns a big string of all the pieces in the text data."
  [{:selected selected :text text :after after}]
  (string text selected (string/reverse after)))

(defn read-file
  [path]
  (with [f (file/open path)]
        (def text (file/read f :all))
        text))

(defn load-file
  [props path]
  (replace-content props (read-file path)))

(defn save-file
  [props path]
  (with [f (file/open path :w)]
        (file/write f (content props))
        (file/flush f)))
