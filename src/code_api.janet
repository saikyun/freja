(import spork/fmt)
(import ./new_gap_buffer :prefix "")

(varfn content
  "Returns a big string of all the pieces in the text data."
  [{:selected selected :text text :after after}]
  (string text selected (string/reverse after)))

(varfn format-code
  [gb]
  (-> gb commit!)
  (def {:caret caret
        :text text} gb)
  (def new-text (fmt/format text))
  (replace-content gb new-text)
  (put-caret gb caret))
