(comment
  ``
Functions other than `commit!` that ends with `!` in this file runs `commit!`.
This means that the function is O(n) where n is the total length of the gap buffer.
The other functions are O(1) or O(n) where n is the length of the gap, which is generally smaller.

Functions with a * after them means something akin to "probably shouldn't use this".
Generally used for internal stuff.
``)

(import spork/test)
(import ./new_gap_buffer_util :prefix "")
(use freja-jaylib)
(import ./find_row_etc :as find)


(defn min*
  "`min` treating `nil` as a high number"
  [a b]
  (if a
    (if b
      (min a b)
      a)
    b))


### iterators

(defmacro gb-iterate
  ``
Iterates over the chars in the gap buffer `gb`.
`start` / `stop` being indexes determining the range of iteration.
`i-sym` will be bound to the index of a char.
`c-sym` will be bound to the current char.
`body` will be run for each char, with `i-sym` / `c-sym` bound.

See `usages/new_gap_buffer.janet` -> ### iterators for examples.
``
  [gb
   start
   stop
   i-sym
   c-sym
   & body]
  ~(label stop-gb-iterate
     (def {:gap-start gap-start
           :gap-stop gap-stop
           :text text
           :gap gap} ,gb)

     (loop [,i-sym :range [(max ,start 0)
                           (min ,stop gap-start)]
            :let [,c-sym (text ,i-sym)]]
       ,;body)

     (loop [,i-sym :range [(max (- ,start gap-start) 0)
                           (min (- ,stop gap-start) (length gap))]
            :let [,c-sym (gap ,i-sym)
                  ,i-sym (+ ,i-sym gap-start)]]
       ,;body)

     (loop [,i-sym :range [(max (+ (- ,start (length gap))
                                   (- gap-stop gap-start))
                                gap-stop)
                           (min (+ (- ,stop (length gap))
                                   (- gap-stop gap-start))
                                (length text))]
            :let [,c-sym (text ,i-sym)
                  ,i-sym (- (+ ,i-sym
                               (length gap))
                            (- gap-stop gap-start))]]
       ,;body)))

(varfn index-char-backward
  "Iterates over gap buffer `gb`, starting from the end and going backwards to the beginning.
Each time called, signals the next index and character."
  [gb]
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :text text
        :gap gap} gb)

  (loop [i :down-to [(dec (length text)) gap-stop]]
    (yield [(+ (- gap-stop gap-start) (length gap) i) # index
            (text i)])) # character

  (loop [i :down-to [(dec (length gap)) 0]]
    (yield [(+ gap-start i)
            (gap i)]))

  (loop [i :down-to [(dec gap-start) 0]]
    (yield [i
            (text i)]))

  nil)

(varfn index-char-backward-start
  ``
Same as `index-char-backward`, but takes a `start` index, from where iteration will begin.
``
  [gb start]
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :text text
        :gap gap} gb)

  (def start start)

  (loop [i :down-to [(min (+ (- start (length gap))
                             (- gap-stop gap-start))
                          (dec (length text))) gap-stop]]
    (yield [(+ (- gap-stop gap-start) (length gap) i) # index
            (text i)])) # character

  (loop [i :down-to [(min (- start gap-start)
                          (dec (length gap)))
                     0]]
    (yield [(+ gap-start i)
            (gap i)]))

  (loop [i :down-to [(min start (dec gap-start)) 0]]
    (yield [i
            (text i)]))

  nil)

(varfn index-char-start
  ``
Same as `index-char-backward-start`, but iterates from beginning to end of `gb`.
``
  [gb start]
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :text text
        :gap gap} gb)

  (loop [i :range [(max 0 start) gap-start]]
    (yield [i
            (text i)]))

  (loop [i :range [(max (- start gap-start)
                        0)

                   (length gap)]]
    (yield [(+ gap-start i)
            (gap i)]))

  (loop [i :range [(max (+ (- start (length gap))
                           (- gap-stop gap-start))
                        gap-stop)
                   (length text)]]
    (yield [(+ (length gap)
               (- gap-stop gap-start)
               i)
            (text i)]))

  nil)

### string functions

(defn gb-length
  ``
Like `length`, but for gap buffers.
``
  [{:text text
    :gap-start gap-start
    :gap-stop gap-stop
    :gap gap}]
  (- (+ (length text)
        (length gap))
     (- gap-stop gap-start)))

(varfn gb-slice
  ``
Like `string/slice`, but for gap buffers.
``
  [gb start stop]
  (assert (<= start stop) "stop is smaller than start")

  (let [{:selection selection} gb]
    (def b (buffer/new (- stop start)))
    (gb-iterate gb
                start stop
                i c
                (buffer/push-byte b c))
    b))

(defn selection-tuple
  "Gets the selection as a [start-index stop-index] tuple."
  [gb]
  (def {:selection selection
        :caret caret} gb)
  (when selection
    (let [sel-start (min selection caret)
          sel-stop (max selection caret)]
      [sel-start sel-stop])))

(defn get-selection
  "Gets the selection as a buffer."
  [gb]
  (gb-slice gb ;(selection-tuple gb)))

(varfn start-stop-text-gap-nth*
  ``
Returns the char at `i`.
This exists so that one doesn't have to get values from the table
in `gb-nth`, e.g. when looping.
``
  [gap-start gap-stop text gap i]
  (cond (< i gap-start)
    (text i)

    (< i (+ (length gap) gap-start))
    (gap (- i gap-start))

    # else
    (get text (+ (- i (length gap) gap-start) gap-stop))))

(varfn gb-nth
  "Returns the char at `i`."
  [{:gap-start gap-start
    :gap-stop gap-stop
    :gap gap
    :text text} i]
  (start-stop-text-gap-nth* gap-start gap-stop text gap i))


### committing

(defn fresh?
  ``
Returns true if no mutation has happened to the gap.
``
  [{:gap-start gap-start
    :gap-stop gap-stop
    :gap gap}]
  (and (empty? gap)
       (= gap-start gap-stop)))

(varfn commit!
  ``
Cleans up the gap `:gap` (emptying it, setting gap start / stop to be the same),
and updates the text `:text`, inserting / removing into it depending on how the gap looked.
E.g. if insertions were made, they end up in the gap. Those are then blitted into text
when running `commit!`.
If deletions where made, the start / stop of the gap would have been modified,
which leads to parts of text being deleted when running `commit!`.

The purpose of `commit!` is to allow the user to use `(gb :text)` as a regular string,
e.g. when using pegs.
`commit!` is O(n), so you might want to be careful with big files (>1gb) .
In general, most operations try avoid committing, but when wanting a linear string,
it is unavoidable.

There are many usages of `commit!` in `usages/new_gap_buffer.janet`.
``
  [gb]
  (if (fresh? gb)
    gb
    (do (def {:text text
              :gap-start gap-start
              :gap-stop gap-stop
              :gap gap} gb)
      (def b (buffer/new (gb-length gb)))
      (buffer/blit b text 0 0 gap-start)
      (buffer/blit b gap (length b))
      (buffer/blit b text (length b) gap-stop)
      (put gb :text b)
      (let [p (+ gap-start (length gap))]
        (put gb :gap-start p)
        (put gb :gap-stop p))
      (update gb :gap buffer/clear))))


### bounds-check

(defn bounds-check
  "Returns `i` that is greather than or equal to 0, and lesser than the length of the gap buffer."
  [gb i]
  (min (gb-length gb) (max 0 i)))

### caret movement

(varfn put-caret
  ``
Moves the caret in gap buffer `gb` to `i`.
Is bounds checked.
Triggers rerender of caret (`:changed-nav true`).
If selection is equal to caret, delete the selection (i.e. nothing is selected).
``
  [gb i]
  (-> gb
      (put :caret (bounds-check gb i))
      (put :changed-nav true))

  (def {:selection selection
        :caret caret} gb)

  (when (and selection (= selection caret))
    (put gb :selection nil))

  gb)

(varfn update-caret
  ``
Is to `update` what `put-caret` is to `put`.
``
  [gb f & args]
  (put-caret gb (f (gb :caret) ;args)))

### c->s

(defn c->s
  "Makes a string out of a char `c`."
  [c]
  (-> (buffer/new 1)
      (buffer/push-byte c)))

### searching

(def space (first " "))
(def newline (first "\n"))

(varfn word-delimiter?
  "Returns true when char `c` is a word delimiter."
  [c]
  (or (= c space)
      (= c newline)
      (= c (chr "#"))
      (= c (chr "-"))
      (= c (chr `"`))
      (= c (chr "."))
      (= c (chr "/"))
      (= c (chr "("))
      (= c (chr ")"))
      (= c (chr "["))
      (= c (chr "]"))
      (= c (chr "{"))
      (= c (chr "}"))
      (= c (chr "<"))
      (= c (chr ">"))
      (= c (chr "@"))))

(varfn search-forward
  "Gets the position of the end of the word after the start.
Doesn't skip delimiters in the beginning."
  [gb pred start]
  (var target-i -1)

  (def f (fiber/new (fn [] (index-char-start gb start))))

  (loop [[i c] :iterate (resume f)]
    (when (pred c)
      (break))

    (set target-i i))

  (cond (= -1 target-i)
    start

    (> target-i (gb-length gb))
    (gb-length gb)

    # the i we got was the position of the char  
    # the caret position should be one higher than that    
    # i.e. to the right of the char    
    (inc target-i)))

(comment
  (search-forward (string->gb "12|34 67") word-delimiter? 3)
  #=> 4

  (search-forward (string->gb "12|34 67") word-delimiter? 5)
  #=> 7

  (search-forward (string->gb "12|34 67") word-delimiter? 4)
  #=> 4

  (search-forward (string->gb "12|34 67") word-delimiter? 7)
  #=> 7
)

(varfn search-backward
  "Gets the position of the start of the word before start.
Doesn't skip delimiters in the beginning."
  [gb pred start]
  (var target-i -1)

  (def f (fiber/new (fn [] (index-char-backward-start gb (max 0 (dec start))))))

  (loop [[i c] :iterate (resume f)]
    (when (pred c)
      (break))

    (set target-i i))

  (cond (= -1 target-i)
    start

    (> target-i (gb-length gb))
    (gb-length gb)

    target-i))

(comment
  (search-backward (string->gb "12|34 67") word-delimiter? 5)
  #=> 5

  (search-backward (string->gb "12|34 67") word-delimiter? 1)
  #=> 0

  (search-backward (string->gb "12|34 67") word-delimiter? 4)
  #=> 0

  (search-backward (string->gb "12|34 67") word-delimiter? 0)
  #=> 0
)

(varfn word-at-index
  [gb i]
  (let [start (search-backward gb word-delimiter? i)
        stop (search-forward gb word-delimiter? i)]
    [start stop]))

(comment
  (word-at-index (string->gb "12|34 67") 0) #=> [0 4]
  (word-at-index (string->gb "12|34 67") 1) #=> [0 4]
  (word-at-index (string->gb "12|34 67") 2) #=> [0 4]
  (word-at-index (string->gb "12|34 67") 3) #=> [0 4]
  (word-at-index (string->gb "12|34 67") 4) #=> [0 4]
  (word-at-index (string->gb "12|34 67") 5) #=> [5 7]
  (word-at-index (string->gb "12|34 67") 6) #=> [5 7]
  (word-at-index (string->gb "12|34 67") 7) #=> [5 7]
)

(varfn end-of-next-word
  "Gets the position of the end of the word after the caret."
  [gb]
  (def {:caret caret} gb)

  (var target-i caret)

  (def f (fiber/new (fn [] (index-char-start gb target-i))))

  (var skipping-delims true)

  (loop [[i c] :iterate (resume f)
         :let [delim (word-delimiter? c)]]
    # skip word delimiters until encountering a non word delimiter
    (set skipping-delims (and skipping-delims delim))
    (when (not (or (and skipping-delims delim)
                   (and (not skipping-delims)
                        (not delim))))
      (break))

    (set target-i i))

  # the i we got was the position of the char  
  # the caret position should be one higher than that  
  # i.e. to the right of the char  
  (set target-i (inc target-i))

  (if (> target-i (gb-length gb))
    nil
    target-i))

(varfn start-of-previous-word
  "Gets the position of the start of the word before the caret."
  [gb]
  (def {:caret caret} gb)

  ## the caret pos is between two chars  
  # so if we want to go to the char to the left, we decrease the caret by one  
  (var target-i (dec caret))

  (def f (fiber/new (fn [] (index-char-backward-start gb target-i))))

  (var skipping-delims true)

  (loop [[i c] :iterate (resume f)
         :let [delim (word-delimiter? c)]]
    # skip word delimiters until encountering a non word delimiter
    (set skipping-delims (and skipping-delims delim))
    (when (not (or (and skipping-delims delim)
                   (and (not skipping-delims)
                        (not delim))))
      (break))

    (set target-i i))

  (if (= -1 target-i)
    nil
    target-i))

### selection funcs

(varfn deselect
  "Deletes any selection."
  [gb]
  (if (gb :selection)
    (-> gb
        (put :selection nil)
        (put :changed-selection true))
    gb))

(varfn select-all
  "Selects all text in the buffer."
  [gb]
  (-> gb
      (put-caret (gb-length gb))
      (put :selection 0)
      (put :changed-selection true)))

(varfn select-region
  "Selects region."
  [gb start stop]
  (-> gb
      (put-caret stop)
      (put :selection start)
      (put :changed-selection true)))

(varfn select-forward-word
  "Selects the word after the cursor."
  [gb]
  (unless (gb :selection)
    (put gb :selection (gb :caret)))

  (-> gb
      (put-caret (end-of-next-word gb))
      (put :changed-x-pos true)
      (put :changed-selection true)))

(varfn select-backward-word
  "Selects the word before the cursor."
  [gb]
  (if-let [start (start-of-previous-word gb)]
    (do (unless (gb :selection)
          (put gb :selection (gb :caret)))

      (-> gb
          (put-caret (start-of-previous-word gb))
          (put :changed-x-pos true)
          (put :changed-selection true)))
    gb))

(varfn select-forward-char
  "Selects the char after the cursor."
  [gb]
  (def {:selection selection
        :caret caret} gb)

  (when (not selection)
    (put gb :selection caret))

  (-> gb
      (update-caret inc)
      (put :changed-x-pos true)
      (put :changed-selection true)))

(varfn select-backward-char
  "Selects the char after the cursor."
  [gb]
  (def {:selection selection
        :caret caret}
    (commit! gb))

  (when (not selection)
    (put gb :selection caret))

  (-> gb
      (update-caret dec)
      (put :changed-x-pos true)
      (put :changed-selection true)))

(defn index-start-of-line
  ``
  Returns the index of the newline before i.
  ``
  [gb i]
  (search-backward gb |(= $ (chr "\n")) i))

(defn index-end-of-line
  ``
  Returns the index of the newline after i.
  ``
  [gb i]
  (search-forward gb |(= $ (chr "\n")) i))

(varfn beginning-of-line?
  ``
Returns true if the caret is at the beginning of the line.
``
  [gb]
  (= (gb :caret)
     (index-start-of-line gb (gb :caret))))

(varfn end-of-line?
  ``
Returns true if the caret is at the end of the line.
``
  [gb]
  (= (gb :caret)
     (index-end-of-line gb (gb :caret))))

(varfn beginning-of-line
  ``
Moves the caret to the beginning of the line.
``
  [gb]
  (put-caret gb (index-start-of-line gb (gb :caret))))

(varfn end-of-line
  ``
Moves the caret to the end of the line.
``
  [gb]
  (put-caret gb (index-end-of-line gb (gb :caret))))

### gap movement

(varfn put-gap-pos!
  "Commits then puts `pos` into :gap-start & :gap-stop.
Does bounds check as well."
  [gb pos]
  (def {:gap-start gap-start
        :gap-stop gap-stop}
    (commit! gb))

  (def new-pos (bounds-check gb pos))

  (-> gb
      (put :changed-nav true) ## not sure if this is needed anymore
      (put :gap-start new-pos)
      (put :gap-stop new-pos)))

(varfn update-gap-pos!
  "Commits then puts `(f (gb :gap-start))` into :gap-start & :gap-stop.
Does bounds check as well."
  [gb f]
  (def {:gap-start gap-start
        :gap-stop gap-stop}
    (commit! gb))

  (def new-pos (bounds-check gb (f gap-start)))

  (-> gb
      (put :changed-nav true)
      deselect
      (put :gap-start new-pos)
      (put :gap-stop new-pos)))

(varfn move-gap-to-pos!
  "Moves the gap to envelop the pos, if needed."
  [gb pos]
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :gap gap} gb)

  (if (or (< pos (+ gap-start (length gap)))
          (> pos (+ gap-start (length gap))))
    (put-gap-pos! gb pos)
    gb))

(varfn move-gap-to-caret!
  "Moves the gap to envelop the caret, if needed."
  [gb]
  (move-gap-to-pos! gb (gb :caret)))

### removal

(varfn delete-region*
  "Deletes region, without updating caret etc. You should probably use `delete-region!` instead."
  [gb start stop]
  (let [gb (commit! gb)
        {:caret caret} gb]
    (-> gb
        (put :gap-start start)
        (put :gap-stop stop)
        (put :ever-modified true)
        (update :lowest-changed-at min* start)
        commit!)))

(varfn delete-region!
  "Deletes a region between `start` and `stop`.
Updates caret etc as expected."
  [gb start stop]
  (let [{:caret caret
         :selection selection} gb
        to-delete (gb-slice gb start stop)
        gb (delete-region* gb start stop)
        new-caret-pos (cond
                        (>= caret stop)
                        (- caret (- stop start))

                        (>= caret start)
                        start

                        caret)
        gb (-> gb
               (put :selection nil)
               (put-caret new-caret-pos)
               (put :changed-x-pos true)
               (put :changed true))]
    (-> gb
        (put :ever-modified true)
        (update :lowest-changed-at min* start)
        (update :actions array/push
                {:kind :delete
                 :start start
                 :stop stop
                 :caret-before caret
                 :selection-before selection
                 :content to-delete
                 :caret-after (gb :caret)
                 :selection-after (gb :selection)}))))

(varfn delete-selection!
  "Just runs `delete-region!` with the `selection` / `caret` as arguments."
  [gb]
  (def {:selection selection
        :caret caret} gb)
  (if selection
    (let [sel-start (min selection caret)
          sel-stop (max selection caret)]
      (delete-region! gb sel-start sel-stop))
    gb))

(varfn delete-before-caret!
  "Deletes the char before the caret (i.e. backspace)."
  [gb]
  (let [{:caret caret
         :gap-start gap-start
         :selection selection
         :gap gap} (move-gap-to-caret! gb)
        gap-i (- caret gap-start)
        to-delete (gb-slice gb (dec (gb :caret)) (gb :caret))]
    (when (and (not selection)
               (= (gb :caret) 0))
      (break gb))

    (if selection
      (delete-selection! gb)
      (do
        (if (empty? gap)
          (-> gb
              (put :changed true)
              (put :changed-x-pos true)
              (update :gap-start |(max 0 (dec $)))
              (update :caret |(max 0 (dec $))))
          (-> gb
              (put :changed true)
              (put :changed-x-pos true)
              (update :gap
                      (fn [gap]
                        (let [to-the-right (buffer/slice gap gap-i)]
                          (buffer/popn gap (inc (- (length gap) gap-i)))
                          (buffer/push-string gap to-the-right))))
              (update :caret |(max 0 (dec $)))))

        (-> gb
            (put :ever-modified true)
            (update :lowest-changed-at min* (gb :caret))
            (update :actions array/push
                    {:kind :delete
                     :start (gb :caret)
                     :stop caret
                     :caret-before caret
                     :selection-before selection
                     :content to-delete
                     :caret-after (gb :caret)
                     :selection-after (gb :selection)}))))))

(varfn delete-word-forward!
  "Deletes the word after the cursor."
  [gb]

  (def {:selection selection
        :caret caret} gb)

  (if selection
    (delete-selection! gb)
    (if-let [stop (end-of-next-word gb)]
      (-> gb
          (delete-region! caret stop)
          (put :changed-x-pos true))
      gb)))

(varfn delete-word-backward!
  "Deletes the word before the cursor."
  [gb]

  (def {:selection selection
        :caret caret} gb)

  (if selection
    (delete-selection! gb)
    (if-let [start (start-of-previous-word gb)]
      (-> gb
          (delete-region! start caret)
          (put :changed-x-pos true))
      gb)))

(varfn delete-after-caret!
  "Deletes the character after the cursor."
  [gb]

  (def {:selection selection
        :caret caret} gb)

  (cond selection
    (delete-selection! gb)

    (= caret (gb-length gb))
    gb

    (-> gb
        (delete-region! caret (inc caret))
        (put :changed-x-pos true))))

(varfn backspace!
  "Deletes selection if any, otherwise runs `delete-before-caret!`."
  [gb]
  (def {:selection selection} gb)

  (cond selection
    (delete-selection! gb)

    # else
    (delete-before-caret! gb))
  (put gb :changed true))

(varfn replace-content
  "Deletes everything in gap buffer `gb`, and replaces it with `text`.
Used e.g. when loading a file."
  [gb text]
  (-> gb
      (update :gap buffer/clear)
      (update :text (comp |(buffer/push-string $ text) buffer/clear))
      (put :actions @[])
      (update :lines array/clear)
      (update :y-poses array/clear)
      (update :line-flags array/clear)
      (update :line-numbers array/clear)
      (put :caret 0)
      (put :redo-queue @[])
      (put :changed true)
      (put :gap-start 0)
      (put :gap-stop 0)))

### insertion

(varfn insert-char-at-pos*
  "Inserts char `c` at the index of `pos`. Won't push undo stack."
  [gb pos c]
  (let [{:gap-start gap-start
         :gap gap} (move-gap-to-pos! gb pos)
        gap-i (- pos gap-start)]
    (-> gb
        (put :ever-modified true)
        (update :lowest-changed-to min* pos)
        (update :gap
                (fn [gap]
                  (let [to-the-right (buffer/slice gap gap-i)]
                    (buffer/popn gap (- (length gap) gap-i))
                    (buffer/push-byte gap c)
                    (buffer/push-string gap to-the-right)))))))

(varfn insert-char-at-caret
  "Inserts char `c` at the index of the caret."
  [gb c]
  (let [{:caret caret-before
         :gap-start gap-start
         :gap gap} (move-gap-to-caret! gb)
        gap-i (- caret-before gap-start)
        gb (-> gb
               (update :gap
                       (fn [gap]
                         (let [to-the-right (buffer/slice gap gap-i)]
                           (buffer/popn gap (- (length gap) gap-i))
                           (buffer/push-byte gap c)
                           (buffer/push-string gap to-the-right))))
               (update :caret inc))]
    (-> gb
        (put :ever-modified true)
        (update :lowest-changed-at min* caret-before)
        (update
          :actions
          array/push
          {:kind :insert
           :caret-before caret-before
           :content (c->s c)
           :caret-after (gb :caret)
           :start caret-before
           :stop (gb :caret)}))))

(varfn insert-string-at-pos*
  "Inserts string `s` at the `pos`, without updating the caret or adding to the history.
You should probably use `insert-string-at-pos!` instead."
  [gb pos s]
  (let [{:gap-start gap-start
         :gap gap} (move-gap-to-pos! gb pos)
        gap-i (- pos gap-start)]
    (-> gb
        (put :ever-modified true)
        (update :lowest-changed-to min* pos)
        (update :gap
                (fn [gap]
                  (let [to-the-right (buffer/slice gap gap-i)]
                    (buffer/popn gap (- (length gap) gap-i))
                    (buffer/push-string gap s)
                    (buffer/push-string gap to-the-right)))))))

(varfn insert-string-at-caret*
  "Inserts string `s` at the position of the caret, without updating the caret or adding to the history.
You should probably use `insert-string-at-caret!` instead."
  [gb s]
  (insert-string-at-pos* gb (gb :caret) s))

(varfn insert-string-at-pos!
  "Inserts string `s` at `pos`."
  [gb pos s]
  (let [caret (gb :caret)
        gb (insert-string-at-pos* gb pos s)]

    (-> gb
        (put :ever-modified true)
        (update :lowest-changed-at min* pos)
        (put :changed-x-pos true)
        (put :changed true)
        (update
          :actions
          array/push
          {:kind :insert
           :caret-before caret
           :content s
           :caret-after caret
           :start pos
           :stop (+ pos (length s))}))))

(defn append-char*
  "Inserts char `c` at the end of `gb`. Won't push undo stack."
  [gb c]
  (insert-char-at-pos* gb (gb-length gb) c))

(defn append-string!
  "Inserts string `s` at the end of `gb`."
  [gb s]
  (insert-string-at-pos! gb (gb-length gb) s))

(varfn insert-string-at-caret!
  "Inserts string `s` at the position of the caret."
  [gb s]
  (let [{:caret caret-before} gb
        gb (-> gb
               (insert-string-at-caret* s)
               (update :caret + (length s)))]

    (-> gb
        (put :ever-modified true)
        (update :lowest-changed-at min* caret-before)
        (put :changed-x-pos true)
        (put :changed true)
        (update
          :actions
          array/push
          {:kind :insert
           :caret-before caret-before
           :content s
           :caret-after (gb :caret)
           :start caret-before
           :stop (gb :caret)}))))

(varfn insert-char!
  "Inserts at caret based on `k`, which generally comes from `jaylib/get-key-pressed`.
Deletes selection."
  [gb k]
  (def gb (delete-selection! gb))

  (let [k (case k
            :backslash (chr "\\")
            :space (chr " ")
            :grave (chr "`")
            :left-bracket (chr "[")
            :right-bracket (chr "]")
            (if (keyword? k)
              (first k)
              k))]
    (insert-char-at-caret gb k))

  (-> gb
      (put :changed-x-pos true)
      (put :changed true)))

(defn ascii-upper-chr*
  "Uppercases an ascii char between a-z."
  [c]
  (if (and (>= c 97)
           (<= c 122))
    (- c 32)
    c))

(varfn insert-char-upper!
  "Like `insert-char!`, but uppercases a-z."
  [gb k]
  (def gb (delete-selection! gb))

  (let [k (case k
            :space (chr " ")
            :grave (chr "`")
            :left-bracket (chr "[")
            :right-bracket (chr "]")
            (if (keyword? k)
              (ascii-upper-chr* (first k))
              k))]
    (insert-char-at-caret gb k))

  (-> gb
      (put :changed-x-pos true)
      (put :changed true)))

### navigation

(varfn forward-word
  "Moves the caret to the end of the word after the caret."
  [gb]
  (-> gb
      deselect
      (put-caret (end-of-next-word gb))
      (put :changed-x-pos true)))

(varfn backward-word
  "Moves the caret to the beginning of the word before the caret."
  [gb]
  (deselect gb)

  (if-let [start (start-of-previous-word gb)]
    (-> gb
        (put-caret start)
        (put :changed-x-pos true))
    gb))

(varfn end-of-buffer
  "Moves the caret to end of the buffer."
  [gb]
  (-> gb
      deselect
      (put-caret (gb-length gb))
      (put :changed-x-pos true)))

(varfn beginning-of-buffer
  "Moves the caret to beginning of the buffer."
  [gb]
  (-> gb
      deselect
      (put-caret 0)
      (put :changed-x-pos true)))

(varfn forward-char
  "If selection is made, clears the selection.
Otherwise moves the caret forward one character."
  [gb]
  (if (gb :selection)
    (deselect gb)
    (-> gb
        (update-caret inc)
        #(put :stickiness :right)
        (put :changed-nav true)
        (put :changed-x-pos true))))

(varfn backward-char
  "If selection is made, clears the selection.
Otherwise moves the caret backward one character."
  [gb]
  (if (gb :selection)
    (deselect gb)
    (-> gb
        (update-caret dec)
        #(put :stickiness :down)
        (put :changed-nav true)
        (put :changed-x-pos true))))

(varfn move-n
  "Moves the caret `n` characters."
  [gb n]
  (-> gb
      deselect
      (update-caret + n)
      (put :stickiness :down)
      (put :changed-nav true)
      (put :changed-x-pos true)))


### cut / copy / paste

(varfn get-selection
  "Runs `gb-slice` on the selection."
  [gb]
  (def {:selection selection} gb)
  (when selection
    (let [{:caret caret} gb
          sel-start (min selection caret)
          sel-stop (max selection caret)]
      (gb-slice gb sel-start sel-stop))))

(varfn copy
  "Copies selected text into clipboard."
  [gb]
  (when-let [s (get-selection gb)]
    (set-clipboard-text (string s)))
  gb)

(varfn cut!
  "Cuts selected text into clipboard."
  [gb]
  (-> gb
      copy
      delete-selection!))

(varfn paste!
  "Pastes from clipboard."
  [gb]
  (-> gb
      delete-selection!
      (insert-string-at-caret! (get-clipboard-text))))


### undo

(varfn undo!
  [gb]
  (if-let [action (array/pop (gb :actions))]
    (let [{:content c
           :caret-after i-after
           :caret-before i-before
           :selection-after sel-after
           :selection-before sel-before
           :kind kind
           :start start
           :stop stop} action]

      (case kind
        :insert (-> gb
                    (delete-region* start stop)
                    (put :caret i-before)
                    (put :selection sel-before)
                    (put :changed true))
        :delete (-> gb
                    (put :caret start)
                    (insert-string-at-caret* c)
                    (put :caret i-before)
                    (put :selection sel-before)
                    (put :changed true))
        (do (update gb :actions array/push action)
          (error (string "Not supported kind: " kind))))

      (array/push (gb :redo-queue) action)

      gb)
    gb))

(varfn redo!
  [gb]
  (if-let [action (array/pop (gb :redo-queue))]
    (let [{:content c
           :caret-after i-after
           :caret-before i-before
           :selection-after sel-after
           :selection-before sel-before
           :kind kind
           :start start
           :stop stop} action]

      (case kind
        :delete (-> gb
                    (delete-region* start stop)
                    (put :caret i-after)
                    (put :changed true))

        :insert (-> gb
                    (put :caret start)
                    (insert-string-at-caret* c)
                    (put :caret i-after)
                    (put :selection sel-after)
                    (put :changed true))
        (do (update gb :redo-queue array/push action)
          (error (string "Not supported kind: " kind))))

      (array/push (gb :actions) action)

      gb)
    gb))

(varfn content
  [gb]
  (-> gb
      commit!
      (get :text)))

### search
(varfn finder
  [str]
  (peg/compile ~(any (+ (/ (* ($) ,str ($)) ,(fn [& args] args)) 1))))

(varfn find-paragraphs
  [s]
  (peg/match ~{:double-newline (at-least 2 "\n")
               :other (if-not :double-newline 1)
               :main (any (* (+ (* ""
                                   (not :double-newline))
                                (* (any :other)
                                   :double-newline))
                             (/ (* ($)
                                   (some :other)
                                   ($))
                                ,tuple)
                             (+ (* (not :double-newline)
                                   -1)
                                (* (any :other)
                                   :double-newline))))}
             s))

(comment
  (find-paragraphs "aoe\n\naohnetsnhtsoae\n\nao\ne")
  #=> @[(0 3) (5 19) (21 25)]

  (find-paragraphs "\n\naoe\n\n")
  #=> @[(2 5)]

  (find-paragraphs "\n\naoe")
  #=> @[(2 5)]

  (find-paragraphs "aoe")
  #=> @[(0 3)]

  (find-paragraphs "aoe\n\n")
  #=> @[(0 3)]
)

(varfn find-surrounding-paragraph!
  [gb index]
  # if index is before the first paragraph, we want the first paragraph
  (def pgs (find-paragraphs (content gb)))
  (var res (first pgs))
  (loop [pg :in pgs
         :let [[start stop] pg]
         :until (> start index)]
    (set res pg))
  res)

(comment
  (def gb (string->gb "\n\naoe\n\naohnetsnhtsoae\n\naoe"))

  (find-paragraphs (content gb))

  (gb-find-surrounding-paragraph! gb 3))

(varfn gb-find!
  [gb peg]
  (-?> (peg/match (finder peg) (content gb))
       first))

(varfn gb-find-forward!
  [gb peg]
  (or (-?> (peg/match (finder peg) (content gb) (gb :caret))
           first
           last)
      (-?> (peg/match (finder peg) (content gb) 0)
           first
           last)))


(varfn gb-find2!
  [gb peg]
  (def matches (peg/match (finder peg) (content gb) 0))

  [(find/binary-search-closest matches |(compare (gb :caret) (first $))) matches])

(varfn gb-find-backward!
  [gb peg]
  (or (-?> (peg/match (finder peg) (string/slice (content gb) 0 (gb :caret)))
           last
           first)

      (-?> (peg/match (finder peg) (content gb))
           last
           first)))

(comment
  (gb-find! (string->gb "abc\n123 hej WAT") "hej")

  (peg/match (finder "a") "bac" 0))

(def- column-peg
  (peg/compile ~{:back (> -1 (+ (* (set "\r\n") ($)) :back))
                 :main :back}))

(defn column
  ``
  Returns column (nof characters from left newline) for caret position `i`.
  ``
  [s i]
  (let [pos (min (length s) i)]
    (if-let [[n] (peg/match column-peg s pos)]
      (- pos n)
      # no match means we're at first line
      pos)))

(defn column!
  ``
  Wrapper for `column` for gap buffers.
  ``
  [gb i]
  (column (content gb) i))

(comment
  (import freja/state)

  (def gb (get-in state/editor-state [:left-state :editor :gb]))
  (column! gb 13)

  (column ``
a1
2
322
``
          6)
  #=> 1
)

(defn comment-line
  ``
  Toggles comment for a single line.
  Inserts a "#" at the beginning of a line,
  unless there is already a "#" at the beginning,
  then it removes the "#" instead.
  ``
  [gb]
  (let [col (column! gb (gb :caret))
        start-of-line (- (gb :caret) col)]
    (if (= (chr "#") (in (content gb) start-of-line))
      (do
        (delete-region! gb start-of-line (inc start-of-line))
        (when (= col 1)
          (put gb :stickiness :down)))
      (do
        (insert-string-at-pos! gb start-of-line "#")
        (move-n gb 1)))))

(defn index-of-line
  ``
  Takes 0-indexed line number and returns caret position of the beginning of that line.
  Returns `nil` if the line isn't found. (E.g. `line` is greater than total nof lines)
  ``
  [gb line]
  (var ix nil)
  (var nof-lines 0)
  (gb-iterate gb
              0 (gb-length gb)
              i c
              (when (= c (chr "\n"))
                (++ nof-lines))
              (when (= nof-lines line)
                (set ix (inc i)) # right after the \n character
                (return stop-gb-iterate)))
  ix)

(defn line-number
  ``
  Returns 0-indexed line number for caret position `pos`.
  ``
  [gb pos]
  (var n 0)
  (gb-iterate gb
              0 pos
              i c
              (when (= c (chr "\n"))
                (++ n)))
  n)

(defn current-line-number
  ``
  Returns 0-indexed line number for current caret position.
  ``
  [gb]
  (line-number gb (gb :caret)))

### initialization

(varfn new-gap-buffer
  []
  @{:text @""
    :gap-start 0
    :gap-stop 0
    :gap @""
    :caret 0

    :actions @[]
    :redo-queue @[]

    :styling @[]

    :selection nil

    :size [100 100]
    :position [0 0]
    :offset [0 0]

    :changed true
    :scroll 0
    :blink 0

    :not-changed-timer 0

    :hooks/new-line @[]
    :hooks/invalidate-cache @[]

    :lines @[]
    :y-poses @[]
    :line-flags @[]
    :line-numbers @[]})


(defn remove-hook
  ``
removes a hook named by k, of type hook-name from gb
``
  [gb hook-name k]
  (update gb hook-name
          (fn [hooks]
            (if-let [i (find-index |(= k (first $)) hooks)]
              (array/remove hooks i)
              hooks))))

(defn add-hook
  ``
adds a hook with name k
to the type hook-name in gb
``
  [gb hook-name k hook]

  (def hs (in gb hook-name))
  (assert hs (string/format
               "%p is not a valid hook. valid hooks are: %p"
               hook-name
               (filter |(string/has-prefix? "hooks/" $) (keys gb))))

  (assert (function? hook) "hook must be a function")

  (remove-hook gb hook-name k)

  (array/push hs [k hook])
  #
)


### render for debugging

(varfn render
  "Creates a textual representation of a gap buffer.
Should probably be in new_gap_buffer_util, but it depends on functions in this file."
  [gb]
  (def b (buffer/new (gb-length gb)))

  (gb-iterate
    gb
    0 (gb-length gb)
    i c

    (when (= i (gb :caret))
      (buffer/push-byte b (chr "|")))

    (when (= i (gb :selection))
      (buffer/push-byte b (chr "*")))

    (buffer/push-byte b c))

  (when (= (gb-length gb) (gb :caret))
    (buffer/push-byte b (chr "|")))

  (when (= (gb-length gb) (gb :selection))
    (buffer/push-byte b (chr "*")))

  (string b))
