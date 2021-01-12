(import spork/fmt)
(import ./text_api :prefix "")

(varfn content
  "Returns a big string of all the pieces in the text data."
  [{:selected selected :text text :after after}]
  (string text selected (string/reverse after)))

(varfn format-code
  [text-data]
  (def pos (length (text-data :text)))
  (def new-text (fmt/format (content text-data)))
  (buffer/clear (text-data :text))
  (buffer/clear (text-data :selected))
  (buffer/clear (text-data :after))
  (put text-data :text new-text)
  (move-to-pos text-data (min (length (text-data :text)) pos)))
