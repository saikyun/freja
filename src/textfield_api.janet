(use jaylib)

(defn content
  "Returns a big string of all the pieces in the text data."
  [{:selected selected :text text :after after}]
  (string text selected (string/reverse after)))

(defn select-until-beginning
  "Selects all text from cursor to beginning of buffer."
  [text]
  (def {:selected selected :text text} text)
  (put text :dir :left)
  (buffer/push-string selected text)
  (buffer/clear text))

(defn select-until-end
  "Selects all text from cursor to end of buffer."
  [{:selected selected :text text :after after}]
  (put text :dir :right)
  (buffer/push-string selected (string/reverse after))
  (buffer/clear after))

(defn select-region
  "Selects text between index start and index end."
  [text-data start end]
  (let [{:after after} text-data
        both (content text-data)
        [start end] (if (> start end)
                      (do (put text-data :dir :left)
                          [end start])
                      (do (put text-data :dir :right)
                          [start end]))]
    (put text-data :text (buffer/slice both 0 start))
    (put text-data :selected (buffer/slice both start end))
    (buffer/clear after)
    (buffer/push-string after (string/reverse (buffer/slice both end)))))

(defn move-to-pos
  "Selects text between index start and index end."
  [text-data pos]
  (select-region text-data pos pos))

(defn move-to-beginning
  "Moves cursor to beginning of buffer."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (buffer/push-string after (string/reverse selected))
  (buffer/push-string after (string/reverse text))
  (buffer/clear selected)
  (buffer/clear text))

(defn move-to-end
  "Moves cursor to end of buffer."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (buffer/push-string text selected)
  (buffer/push-string text (string/reverse after))
  (buffer/clear selected)
  (buffer/clear after))

(defn copy
  "Copies selected text into clipboard."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (set-clipboard-text (string selected)))

(defn delete-selected
  "Deletes selected text.
  Always run when inserting (e.g. writing chars or when pasting).
  Returns previously selected text.
  Returns `nil` if no text was selected."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (def old selected)
  (put text-data :selected @"")
  (when (not (empty? old))
    old))

(defn cut
  "Cuts selected text into clipboard."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (set-clipboard-text (string selected))
  (delete-selected text-data))

(defn paste
  "Pastes from clipboard."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (delete-selected text-data)
  (buffer/push-string text (get-clipboard-text)))

(defn select-surrounding-word
  "Selects the word surrounding the cursor."
  [text-data]
  (def {:selected selected :text text :after after :dir dir} text-data)
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
  
  (put text-data :dir :right))

(defn select-all
  "Selects all text in buffer."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (put text-data :dir :right)
  (def new-selected (buffer/new (+ (length text)
                                   (length selected)
                                   (length after))))    
  (buffer/push-string new-selected text)    
  (buffer/push-string new-selected selected)    
  (buffer/push-string new-selected (string/reverse after))    
  (put text-data :selected new-selected)    
  (buffer/clear text)    
  (buffer/clear after))

(defn delete-word-before
  "Deletes the word before the cursor.
  If text is selected deletes the selection instead."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  ## TODO: Try putting the exact position modified into changed instead
  ## In order to only rerender a single line
  (put text-data :changed true)
  
  (when (not (delete-selected text-data))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text)))]
      (buffer/popn text l))))

(defn delete-word-after
  "Deletes the word after the cursor.
  If text is selected deletes the selection instead."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (put text-data :changed true)
  (when (not (delete-selected text-data))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse after)))]
      (buffer/popn after l))))

(defn backspace
  "Removes a single character before the cursor.
  If text is selected deletes the selection instead."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (put text-data :changed true)
  (when (not (delete-selected text-data))
    (buffer/popn text 1)))

(defn forward-delete
  "Removes a single character after the cursor.
  If text is selected deletes the selection instead."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (put text-data :changed true)
  (when (not (delete-selected text-data))
    (buffer/popn after 1)))

(defn select-word-before
  "Selects a word before the cursor."
  [text-data]
  (def {:selected selected :text text :after after :dir dir} text-data)
  (if (and (not (empty? selected))       # when text is selected and the direction is right
           (= dir :right))                  # we deselect rather than select
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse selected)))]
      (buffer/push-string after (string/reverse (buffer/slice selected (dec (- l)))))
      (buffer/popn selected l))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text)))]
      (put text-data :dir :left)
      (put text-data :selected (buffer (buffer/slice text (dec (- l))) selected))
      (buffer/popn text l))))

(defn select-word-after
  "Selects a word after the cursor."
  [text-data]
  (def {:selected selected :text text :after after :dir dir} text-data)
  (if (and (not (empty? selected))     # when text is selected and the direction is left
           (= dir :left)) # we deselect rather than select
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) selected))]
      (buffer/push-string text (buffer/slice selected 0 l))
      (put text-data :selected (buffer/slice selected l)))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse after)))]
      (put text-data :dir :right)
      (buffer/push-string selected (string/reverse (buffer/slice after (dec (- l)))))
      (buffer/popn after l))))

(defn move-word-before
  "Moves the cursor one word to the left."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text)))]
    (when (not (empty? selected))
      (buffer/push-string after (string/reverse selected))
      (buffer/clear selected))
    (buffer/push-string after (string/reverse (buffer/slice text (dec (- l)))))
    (buffer/popn text l)))

(defn move-word-after
  "Moves the cursor one word to the right."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse after)))]
    (when (not (empty? selected))
      (buffer/push-string text selected)
      (buffer/clear selected))
    (buffer/push-string text (string/reverse (buffer/slice after (dec (- l)))))
    (buffer/popn after l)))

(defn select-char-before
  "Selects the char before the cursor."
  [text-data]
  (def {:selected selected :text text :after after :dir dir} text-data)
  (if (and (= dir :right)
           (not (empty? selected)))
    (do (put after (length after) (last selected))
        (buffer/popn selected 1))
    (when (not (empty? text))
      (put text-data :dir :left)
      (let [o selected]
        (put text-data :selected (buffer/new (inc (length o))))
        (put (text-data :selected) 0 (last text))
        (buffer/push-string (text-data :selected) o))
      (buffer/popn text 1))))

(defn select-char-after
  "Selects the char after the cursor."
  [text-data]
  (def {:selected selected :text text :after after :dir dir} text-data)
  (if (and (= dir :left)
           (not (empty? selected)))
    (do (put text (length text) (first selected))
        (put text-data :selected (buffer/slice selected 1)))
    (when (not (empty? after))
      (put text-data :dir :right)
      (put selected (length selected) (last after))
      (buffer/popn after 1))))

(defn move-char-before
  "Moves the cursor one char to the left."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (if (not (empty? selected))
    (do (buffer/push-string after (string/reverse selected))
        (buffer/clear selected))
    (when (not (empty? text))
      (put after (length after) (last text))
      (buffer/popn text 1))))

(defn move-char-after
  "Moves the cursor one char to the right."
  [text-data]
  (def {:selected selected :text text :after after} text-data)
  (if (not (empty? selected))
    (do (buffer/push-string text selected)
        (buffer/clear selected))
    (when (not (empty? after))
      (put text (length text) (last after))
      (buffer/popn after 1))))

(defn insert-char
  "Inserts a single char."
  [{:selected selected :text text :after after} k]
  (case k
    :space (buffer/push-string text " ")
    :grave (buffer/push-string text "`")
    :left-bracket (buffer/push-string text "[")
    :right-bracket (buffer/push-string text "]")
    (do (buffer/clear selected)
        (if (keyword? k)
          (buffer/push-string text (string k))
          (put text (length text) k)))))

(defn insert-char-upper
  "Inserts a single uppercase char."
  [{:selected selected :text text :after after} k]
  (case k
    :space (buffer/push-string text " ")
    :grave (buffer/push-string text "`")
    :left-bracket (buffer/push-string text "[")
    :right-bracket (buffer/push-string text "]")
    (do (buffer/clear selected)
        (if (keyword? k)
          (buffer/push-string text (string/ascii-upper (string k)))
          (put text (length text) k)))))
