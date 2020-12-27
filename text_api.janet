(use jaylib)
(import ./text_rendering :prefix "")

(varfn content
  "Returns a big string of all the pieces in the text data."
  [{:selected selected :text text :after after}]
  (string text selected (string/reverse after)))

(varfn select-until-beginning
  "Selects all text from cursor to beginning of buffer."
  [props]
  (def {:selected selected :text text} props)
  (put props :dir :left)
  (buffer/push-string selected text)
  (buffer/clear text)
  (refresh-caret-pos props))

(varfn select-until-end
  "Selects all text from cursor to end of buffer."
  [props]
  (def {:selected selected :text text :after after} props)
  (put text :dir :right)
  (buffer/push-string selected (string/reverse after))
  (buffer/clear after)
  
  (refresh-caret-pos props))

(varfn select-region
  "Selects text between index start and index end."
  [props start end]
  (let [{:after after} props
        both (content props)
        [start end] (if (> start end)
                      (do (put props :dir :left)
                          [end start])
                      (do (put props :dir :right)
                          [start end]))]
    (put props :text (buffer/slice both 0 start))
    (put props :selected (buffer/slice both start end))
    (buffer/clear after)
    (buffer/push-string after (string/reverse (buffer/slice both end)))))

(varfn move-to-pos
  "Selects text between index start and index end."
  [props pos]
  (select-region props pos pos))

(varfn select-until-beginning-of-line
  "Selects all text from cursor to beginning of line."
  [props]
  (def {:text text :rows rows :selected selected :current-row current-row} props)
  (def {:start start} (rows current-row))
  
  (select-region props (+ (length text) (length selected)) start)
  
  (refresh-caret-pos props))

(varfn move-to-beginning
  "Moves cursor to beginning of buffer."
  [props]
  (def {:selected selected :text text :after after} props)
  (buffer/push-string after (string/reverse selected))
  (buffer/push-string after (string/reverse text))
  (buffer/clear selected)
  (buffer/clear text)
  
  (refresh-caret-pos props))

(varfn move-to-beginning-of-line
  "Moves cursor to beginning of line."
  [props]
  (def {:rows rows :current-row current-row} props)
  (def {:start start} (rows current-row))
  
  (move-to-pos props start)
  
  (refresh-caret-pos props))

(varfn select-until-end-of-line
  "Selects all text from cursor to end of line."
  [props]
  (def {:text text :selected selected :rows rows :current-row current-row} props)
  (def {:stop stop} (rows current-row))
  
  (select-region props (length text)
                 (if (= (dec (length rows)) current-row)
                   stop
                   (dec stop)))
  
  (refresh-caret-pos props))

(varfn move-to-end
  "Moves cursor to end of buffer."
  [props]
  (def {:selected selected :text text :after after} props)
  (buffer/push-string text selected)
  (buffer/push-string text (string/reverse after))
  (buffer/clear selected)
  (buffer/clear after)
  
  (refresh-caret-pos props))

(varfn move-to-end-of-line
  "Moves cursor to end of line."
  [props]
  (def {:rows rows :current-row current-row} props)
  (def {:stop stop} (rows current-row))
  
  (move-to-pos props (if (= (dec (length rows)) current-row)
                       stop
                       (dec stop)))
  
  (refresh-caret-pos props))

(varfn copy
  "Copies selected text into clipboard."
  [props]
  (def {:selected selected :text text :after after} props)
  (set-clipboard-text (string selected)))

(varfn delete-selected
  "Deletes selected text.
  Always run when inserting (e.g. writing chars or when pasting).
  Returns previously selected text.
  Returns `nil` if no text was selected."
  [props]
  (def {:selected selected :text text :after after} props)
  (def old selected)
  (put props :selected @"")
  
  (refresh-caret-pos props)
  
  (when (not (empty? old))
    old))

(varfn cut
  "Cuts selected text into clipboard."
  [props]
  (def {:selected selected :text text :after after} props)
  (set-clipboard-text (string selected))
  (delete-selected props)

  (refresh-caret-pos props))

(varfn paste
  "Pastes from clipboard."
  [props]
  (def {:selected selected :text text :after after} props)
  (delete-selected props)
  (buffer/push-string text (get-clipboard-text))
  
  (refresh-caret-pos props))

(varfn select-surrounding-word
  "Selects the word surrounding the cursor."
  [props]
  (def {:selected selected :text text :after after :dir dir} props)
  (if (= dir :right)
    (buffer/push-string text selected)
    (buffer/push-string after (string/reverse selected)))
  (buffer/clear selected)
  
  (def t-l (first (peg/match '(* (any :S) ($)) (string/reverse text))))
  (def at-l (first (peg/match '(* (any :S) ($)) (string/reverse after))))
  
  (buffer/push-string selected (string/slice text (dec (- t-l))))
  (buffer/push-string selected (string/reverse (string/slice after (dec (- at-l)))))
  (buffer/popn text t-l)
  (buffer/popn after at-l)
  
  (put props :dir :right)
  (refresh-caret-pos props))

(varfn select-all
  "Selects all text in buffer."
  [props]
  (def {:selected selected :text text :after after} props)
  (put props :dir :right)
  (def new-selected (buffer/new (+ (length text)
                                   (length selected)
                                   (length after))))    
  (buffer/push-string new-selected text)    
  (buffer/push-string new-selected selected)    
  (buffer/push-string new-selected (string/reverse after))    
  (put props :selected new-selected)    
  (buffer/clear text)    
  (buffer/clear after)
  (refresh-caret-pos props))

(varfn delete-word-before
  "Deletes the word before the cursor.
  If text is selected deletes the selection instead."
  [props]
  (def {:selected selected :text text :after after} props)
  (when (not (delete-selected props))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text)))]
      (buffer/popn text l)))
  (refresh-caret-pos props))

(varfn delete-word-after
  "Deletes the word after the cursor.
  If text is selected deletes the selection instead."
  [props]
  (def {:selected selected :text text :after after} props)
  (when (not (delete-selected props))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse after)))]
      (buffer/popn after l)))
  (refresh-caret-pos props))

(varfn backspace
  "Removes a single character before the cursor.
  If text is selected deletes the selection instead."
  [props]
  (def {:selected selected :text text :after after} props)
  (when (not (delete-selected props))
    (buffer/popn text 1))
  (refresh-caret-pos props))

(varfn forward-delete
  "Removes a single character after the cursor.
  If text is selected deletes the selection instead."
  [props]
  (def {:selected selected :text text :after after} props)
  (when (not (delete-selected props))
    (buffer/popn after 1))
  (refresh-caret-pos props))

(varfn select-word-before
  "Selects a word before the cursor."
  [props]
  (def {:selected selected :text text :after after :dir dir} props)
  (if (and (not (empty? selected))       # when text is selected and the direction is right
           (= dir :right))                  # we deselect rather than select
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse selected)))]
      (buffer/push-string after (string/reverse (buffer/slice selected (dec (- l)))))
      (buffer/popn selected l))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text)))]
      (put props :dir :left)
      (put props :selected (buffer (buffer/slice text (dec (- l))) selected))
      (buffer/popn text l)))
  (refresh-caret-pos props))

(varfn select-word-after
  "Selects a word after the cursor."
  [props]
  (def {:selected selected :text text :after after :dir dir} props)
  (if (and (not (empty? selected))     # when text is selected and the direction is left
           (= dir :left)) # we deselect rather than select
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) selected))]
      (buffer/push-string text (buffer/slice selected 0 l))
      (put props :selected (buffer/slice selected l)))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse after)))]
      (put props :dir :right)
      (buffer/push-string selected (string/reverse (buffer/slice after (dec (- l)))))
      (buffer/popn after l)))
  (refresh-caret-pos props))

(varfn move-word-before
  "Moves the cursor one word to the left."
  [props]
  (def {:selected selected :text text :after after} props)
  (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text)))]
    (when (not (empty? selected))
      (buffer/push-string after (string/reverse selected))
      (buffer/clear selected))
    (buffer/push-string after (string/reverse (buffer/slice text (dec (- l)))))
    (buffer/popn text l))
  (refresh-caret-pos props))

(varfn move-word-after
  "Moves the cursor one word to the right."
  [props]
  (def {:selected selected :text text :after after} props)
  (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse after)))]
    (when (not (empty? selected))
      (buffer/push-string text selected)
      (buffer/clear selected))
    (buffer/push-string text (string/reverse (buffer/slice after (dec (- l)))))
    (buffer/popn after l))
  (refresh-caret-pos props))

(varfn select-char-before
  "Selects the char before the cursor."
  [props]
  (def {:selected selected :text text :after after :dir dir} props)
  (if (and (= dir :right)
           (not (empty? selected)))
    (do (put after (length after) (last selected))
        (buffer/popn selected 1))
    (when (not (empty? text))
      (put props :dir :left)
      (let [o selected]
        (put props :selected (buffer/new (inc (length o))))
        (put (props :selected) 0 (last text))
        (buffer/push-string (props :selected) o))
      (buffer/popn text 1)))
  (refresh-caret-pos props))

(varfn select-char-after
  "Selects the char after the cursor."
  [props]
  (def {:selected selected :text text :after after :dir dir} props)
  (if (and (= dir :left)
           (not (empty? selected)))
    (do (put text (length text) (first selected))
        (put props :selected (buffer/slice selected 1)))
    (when (not (empty? after))
      (put props :dir :right)
      (put selected (length selected) (last after))
      (buffer/popn after 1)))
  (refresh-caret-pos props))

(varfn move-char-before
  "Moves the cursor one char to the left."
  [props]
  (def {:selected selected :text text :after after} props)
  (if (not (empty? selected))
    (do (buffer/push-string after (string/reverse selected))
        (buffer/clear selected))
    (when (not (empty? text))
      (put after (length after) (last text))
      (buffer/popn text 1)))
  (refresh-caret-pos props))

(varfn move-char-after
  "Moves the cursor one char to the right."
  [props]
  (def {:selected selected :text text :after after} props)
  (if (not (empty? selected))
    (do (buffer/push-string text selected)
        (buffer/clear selected))
    (when (not (empty? after))
      (put text (length text) (last after))
      (buffer/popn after 1)))
  (refresh-caret-pos props))

(varfn insert-char
  "Inserts a single char."
  [props k]
  (def {:selected selected :text text :after after} props)
  (case k
    :space (buffer/push-string text " ")
    :grave (buffer/push-string text "`")
    :left-bracket (buffer/push-string text "[")
    :right-bracket (buffer/push-string text "]")
    (do (buffer/clear selected)
        (if (keyword? k)
          (buffer/push-string text (string k))
          (put text (length text) k))))
  (refresh-caret-pos props))

(varfn insert-char-upper
  "Inserts a single uppercase char."
  [props k]
  (def {:selected selected :text text :after after} props)
  (case k
    :space (buffer/push-string text " ")
    :grave (buffer/push-string text "`")
    :left-bracket (buffer/push-string text "[")
    :right-bracket (buffer/push-string text "]")
    (do (buffer/clear selected)
        (if (keyword? k)
          (buffer/push-string text (string/ascii-upper (string k)))
          (put text (length text) k))))
  (refresh-caret-pos props))
