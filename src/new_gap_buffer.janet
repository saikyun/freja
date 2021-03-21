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
(use jaylib)

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
            (text i)]))                               # character
  
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
            (text i)]))                               # character
  
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
      (buffer/blit b text 0           0         gap-start)
      (buffer/blit b gap  (length b)                     )
      (buffer/blit b text (length b)  gap-stop           )
      (put gb :text b)
      (let [p (+ gap-start (length gap))]
        (put gb :gap-start p)
        (put gb :gap-stop  p))
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
  [gb f]
  (put-caret gb (f (gb :caret))))

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
      (= c newline)))

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

(varfn move-gap-to-caret!
  "Moves the gap to envelop the caret, if needed."
  [gb]
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :gap gap
        :caret caret} gb) 
  
  (if (or (< caret (+ gap-start (length gap)))
          (> caret (+ gap-start (length gap))))
    (put-gap-pos! gb caret)
    gb))

### removal

(varfn delete-region*
  "Deletes region, without updating caret etc. You should probably use `delete-region!` instead."
  [gb start stop]
  (let [gb (commit! gb)
        {:caret caret} gb]
    (-> gb
        (put :gap-start start)
        (put :gap-stop stop)
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
               (put :changed true))]
    (update gb :actions array/push
            {:kind :delete
             :start start
             :stop stop
             :caret-before caret
             :selection-before selection
             :content to-delete
             :caret-after (gb :caret)
             :selection-after (gb :selection)})))

(varfn delete-selection!
  "Just runs `delete-region!` with the `selection` / `caret` as arguments."
  [gb]
  (def {:selection selection
        :caret caret} gb)
  (if selection
    (let [sel-start (min selection caret)
          sel-stop  (max selection caret)]
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
    (when (= (gb :caret) 0)
      (break gb))
    
    (if (empty? gap)
      (-> gb
          (update :gap-start |(max 0 (dec $)))
          (update :caret     |(max 0 (dec $))))
      (-> gb
          (update :gap
                  (fn [gap]
                    (let [to-the-right (buffer/slice gap gap-i)]
                      (buffer/popn gap (inc (- (length gap) gap-i)))
                      (buffer/push-string gap to-the-right))))
          (update :caret |(max 0 (dec $)))))
    
    (update gb :actions array/push
            {:kind :delete
             :start (gb :caret)
             :stop  caret
             :caret-before caret
             :selection-before selection
             :content to-delete
             :caret-after (gb :caret)
             :selection-after (gb :selection)})))

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
      (put :changed true)
      (put :gap-start 0)
      (put :gap-stop 0)))

### insertion

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
    (update gb
            :actions
            array/push
            {:kind :insert
             :caret-before caret-before
             :content      (c->s c)
             :caret-after  (gb :caret)
             :start        caret-before
             :stop         (gb :caret)})))

(varfn insert-string-at-caret*
  "Inserts string `s` at the position of the caret, without updating the caret or adding to the history.
You should probably use `insert-string-at-caret!` instead."
  [gb s]
  (let [{:caret caret
         :gap-start gap-start
         :gap gap} (move-gap-to-caret! gb)
        gap-i (- caret gap-start)]
    (update gb :gap
            (fn [gap]
              (let [to-the-right (buffer/slice gap gap-i)]
                (buffer/popn gap (- (length gap) gap-i))
                (buffer/push-string gap s)
                (buffer/push-string gap to-the-right))))))

(varfn insert-string-at-caret!
  "Inserts string `s` at the position of the caret."
  [gb s]
  (let [{:caret caret-before} gb
        gb (-> gb
               (insert-string-at-caret* s)
               (update :caret + (length s)))]
    
    (update gb
            :actions
            array/push
            {:kind :insert
             :caret-before caret-before
             :content      s
             :caret-after  (gb :caret)
             :start        caret-before
             :stop         (gb :caret)})))

(varfn insert-char!
  "Inserts at caret based on `k`, which generally comes from `jaylib/get-key-pressed`.
Deletes selection."
  [gb k]
  (def gb (delete-selection! gb))
  
  (let [k (case k
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

(varfn forward-char
  "If selection is made, clears the selection.
Otherwise moves the caret forward one character."
  [gb]
  (if (gb :selection)
    (deselect gb)
    (-> gb
        (update-caret inc)
        (put :stickiness :right)
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
        (put :stickiness :down)
        (put :changed-nav true)
        (put :changed-x-pos true))))


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
      (insert-string-at-caret! (get-clipboard-text))
      (put :changed-x-pos true)
      (put :changed true)))


### undo

(varfn undo!
  [gb]
  (if-let [action (array/pop (gb :actions))]
    (let [{:content c
           :caret-after  i-after
           :caret-before i-before
           :selection-after sel-after
           :selection-before sel-before
           :kind kind
           :start start
           :stop stop} action]
      
      (case kind
        :insert (-> gb
                    (delete-region* i-before i-after)
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
           :caret-after  i-after
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



### render

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
