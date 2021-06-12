(import spork/fmt)
(import ./new_gap_buffer :prefix "")

(varfn format-code
  [gb]
  (-> gb commit!)
  (def {:caret caret
        :text text} gb)
  (def new-text (fmt/format text))
  (replace-content gb new-text)
  (put-caret gb caret))
