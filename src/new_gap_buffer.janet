(comment
  "Functions other than `commit!` that ends with `!` in this file runs `commit!`.
This means that the function is O(n) where n is the total length of the gap buffer.
The other functions are O(1) or O(n) where n is the length of the gap, which is generally smaller.")

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
  (let [{:selection selection} gb]
    (def b (buffer/new (- stop start)))
    (gb-iterate gb
                start stop
                i c
                (buffer/push-byte b c))
    b))


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
Removes selection, if any.
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
which leads to parts of text being removed when running `commit!`.

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

### removal

(varfn remove-region*
  "Removes region, without updating caret etc. You should probably use `remove-region!` instead."
  [gb start stop]
  (let [gb (commit! gb)
        {:caret caret} gb]
    (-> gb
        (put :gap-start start)
        (put :gap-stop stop)
        commit!)))

(varfn remove-region!
  "Removes a region between `start` and `stop`.
Updates caret etc as expected."
  [gb start stop]
  (let [{:caret caret
         :selection selection} gb
        to-remove (gb-slice gb start stop)
        gb (remove-region* gb start stop)
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
            {:kind :remove
             :start start
             :stop stop
             :caret-before caret
             :selection-before selection
             :content to-remove
             :caret-after (gb :caret)
             :selection-after selection})))

(varfn remove-selection!
  "Just runs `remove-region!` with the `selection` / `caret` as arguments."
  [gb]
  (def {:selection selection
        :caret caret} gb)
  (if selection
    (let [sel-start (min selection caret)
          sel-stop  (max selection caret)]
      (remove-region! gb sel-start sel-stop))
    gb))

(varfn kill-word-forward!
  [gb]
  
  (def {:selection selection
        :caret caret} gb)
  
  (if selection
    (remove-selection! gb)
    (if-let [stop (end-of-next-word gb)]
      (-> gb
          (remove-region! caret stop)
          (put :changed-x-pos true))
      gb)))

(varfn kill-word-backward!
  [gb]
  
  (def {:selection selection
        :caret caret} gb)
  
  (if selection
    (remove-selection! gb)
    (if-let [start (start-of-previous-word gb)]
      (-> gb
          (remove-region! start caret)
          (put :changed-x-pos true))
      gb)))






(defn test-blit
  [gb]
  (def {:text text
        :gap-start gap-start
        :gap-stop gap-stop
        :gap gap} gb)
  (def b (buffer/new (gb-length gb)))
  (buffer/blit b text 0          0 gap-start)
  (buffer/blit b gap  (length b)            )
  (buffer/blit b text (length b)   gap-stop  )
  b
  )

(varfn start-stop-text-gap-nth
  [gap-start gap-stop text gap i]
  (cond (< i gap-start)
    (text i)
    
    (< i (+ (length gap) gap-start))
    (gap (- i gap-start))
    
    # else
    (get text (+ (- i (length gap) gap-start) gap-stop))))

(varfn gb-nth
  [{:gap-start gap-start
    :gap-stop gap-stop
    :gap gap 
    :text text} i]
  (start-stop-text-gap-nth gap-start gap-stop text gap i))

(varfn put-gap-pos!
  "Commits then puts `i` into :gap-start & :gap-stop.
Does bounds check as well."
  [gb i]
  (let [{:gap-start gap-start
         :gap-stop gap-stop}  (commit! gb)
        new-pos (bounds-check gb i)]
    (-> gb
        (put :gap-start new-pos)
        (put :gap-stop new-pos))))

(varfn put-gap-pos!
  "Commits then puts `pos` into :gap-start & :gap-stop.
Does bounds check as well."
  [gb pos]
  (def {:gap-start gap-start
        :gap-stop gap-stop}
    (commit! gb))
  
  (def new-pos (bounds-check gb pos))
  
  (-> gb
      (put :changed-nav true)
      (put :gap-start new-pos)
      (put :gap-stop new-pos)))

(varfn deselect
  [gb]
  (if (gb :selection)
    (-> gb
        (put :selection nil)
        (put :changed-selection true))
    gb))

(varfn select-all
  [gb]
  (-> gb
      (put-caret (gb-length gb))
      (put :selection 0)
      (put :changed-selection true)))

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

(varfn replace-content
  [gb text]
  (-> gb
      (update :gap buffer/clear)
      (update :text (comp |(buffer/push-string $ text) buffer/clear))
      (put :changed true)
      (put :gap-start 0)
      (put :gap-stop 0)))

(varfn move-gap-to-caret!
  [gb]
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :gap gap
        :caret caret} gb) 
  
  (if (or (< caret (+ gap-start (length gap)))
          (> caret (+ gap-start (length gap))))
    (put-gap-pos! gb caret)
    gb))

(comment
  (ez-gb gb-data)
  )

(varfn insert-char-at-caret
  [gb c]
  (let [{:caret caret
         :gap-start gap-start
         :gap gap} (move-gap-to-caret! gb)
        gap-i (- caret gap-start)
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
             :caret-before caret
             :content (c->s c)
             :caret-after (gb :caret)})))

(varfn insert-string-at-caret
  [gb s]
  (let [{:caret caret
         :gap-start gap-start
         :gap gap} (move-gap-to-caret! gb)
        gap-i (- caret gap-start)]
    (-> gb
        (update :gap
                (fn [gap]
                  (let [to-the-right (buffer/slice gap gap-i)]
                    (buffer/popn gap (- (length gap) gap-i))
                    (buffer/push-string gap s)
                    (buffer/push-string gap to-the-right))))
        (update :caret + (length s)))))

(varfn remove-at-caret
  [gb]
  (let [{:caret caret
         :gap-start gap-start
         :gap gap} (move-gap-to-caret! gb)
        gap-i (- caret gap-start)]
    

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
          (update :caret |(max 0 (dec $)))))))

(varfn insert-char
  [gb k]
  (def gb (remove-selection! gb))
  
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

(defn ascii-upper-chr
  [c]
  (if (and (>= c 97)
           (<= c 122))
    (- c 32)
    c))

(varfn insert-char-upper
  [gb k]
  (def gb (remove-selection! gb))
  
  (let [k (case k
            :space (chr " ")
            :grave (chr "`")
            :left-bracket (chr "[")
            :right-bracket (chr "]")
            (if (keyword? k)
              (ascii-upper-chr (first k))
              k))]
    (insert-char-at-caret gb k))
  
  (-> gb
      (put :changed true)))

(varfn backspace
  [gb]
  (def {:selection selection} gb)
  
  (cond selection
    (remove-selection! gb)
    
    # else
    (remove-at-caret gb))
  (put gb :changed true))

(varfn delete
  [gb]
  (update gb :gap-stop |(min (length (gb :text)) (inc $))))

(varfn forward-char!
  [gb]
  (-> gb
      deselect
      (update :caret |(min (gb-length gb) (inc $)))
      (put :changed-nav true)
      (put :changed-x-pos true)))

(varfn backward-char!
  [gb]
  (-> gb
      deselect
      (update :caret |(max 0 (dec $)))
      (put :changed-nav true)
      (put :changed-x-pos true)))

(varfn select-forward-char!
  [gb]
  (def {:selection selection
        :caret caret} gb)
  
  (when (not selection)
    (put gb :selection caret))
  
  (-> gb
      (update-caret inc)
      (put :changed-selection true)))

(varfn select-backward-char!
  [gb]
  (def {:selection selection
        :caret caret}
    (commit! gb))
  
  (when (not selection)
    (put gb :selection caret))
  
  (-> gb
      (update-caret dec)
      (put :changed-selection true)))

## possible functions
## move-cursor-to
## get-current-cursor-pos

(varfn caret-pos
  [{:gap gap
    :gap-start gap-start
    :selection selection}]
  (if selection
    selection
    (+ (length gap)
       gap-start)))

(varfn forward-word
  [gb]
  (-> gb
      deselect
      (put-caret (end-of-next-word gb))
      (put :changed-x-pos true)))

(varfn select-forward-word
  [gb]
  
  (unless (gb :selection)
    (put gb :selection (gb :caret)))
  
  (-> gb
      (put-caret (end-of-next-word gb))
      (put :changed-x-pos true)
      (put :changed-selection true)))

(varfn backward-word
  [gb]
  (deselect gb)
  
  (if-let [start (start-of-previous-word gb)]
    (-> gb
        (put-caret start)
        (put :changed-x-pos true))
    gb))

(varfn select-backward-word
  [gb]
  (if-let [start (start-of-previous-word gb)]
    (do (unless (gb :selection)
          (put gb :selection (gb :caret)))
      
      (-> gb
          (put-caret (start-of-previous-word gb))
          (put :changed-x-pos true)
          (put :changed-selection true)))
    gb))

(varfn get-selection
  [gb]
  (def {:selection selection} gb)
  (when selection
    (let [{:caret caret} gb
          sel-start (min selection caret)
          sel-stop (max selection caret)]
      (gb-slice gb sel-start sel-stop))))

(comment
  (get-selection (string->gb "*(a[b]c)|"))
  #=> @"b"
  )

(varfn copy
  "Copies selected text into clipboard."
  [gb]
  (set-clipboard-text (string (get-selection gb)))
  gb)

(varfn cut
  "Cuts selected text into clipboard."
  [gb]
  (-> gb
      copy
      remove-selection!))

(varfn paste
  "Pastes from clipboard."
  [gb]
  (-> gb
      remove-selection!
      (insert-string-at-caret (get-clipboard-text))
      (put :changed true)))

(varfn text-iterator
  "Returns a function which returns one character at the time from the gap buffer."
  [gb]
  (var i 0)
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :text text
        :gap gap} gb)
  
  (fn []
    (let [res (start-stop-text-gap-nth gap-start gap-stop text gap i)]
      (+= i 1)
      res)))

(varfn text-iterator2
  "Like `text-iterator`, but with a state machine."
  [gb]
  (var i 0)
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :text text
        :gap gap} gb)
  
  (var step nil)
  
  (defn step3
    []
    (get text (+ (- i (length gap) gap-start) gap-stop)))
  
  (defn step2
    []
    (if (< i (+ (length gap) gap-start))
      (gap (- i gap-start))
      (do (set step step3)
        (get text (+ (- i (length gap) gap-start) gap-stop)))))  
  
  (defn step1
    []
    (cond (< i gap-start)
      (text i)
      
      (< i (+ (length gap) gap-start))
      (do (set step step2)
        (gap (- i gap-start)))
      
      # else
      (do (set step step3)
        (get text (+ (- i (length gap) gap-start) gap-stop)))))

  (set step step1)  
  
  (fn []
    (let [res (step)]
      (+= i 1)
      res)))

(varfn text-iterator3
  "Like `text-iterator`, implemented as a generator."
  [gb]
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :text text
        :gap gap} gb)
  
  (loop [i :range [0 gap-start]]
    (yield (text i)))
  
  (loop [c :in gap]
    (yield c))  
  
  (loop [i :range [gap-stop (length text)]]
    (yield (text i)))
  
  nil)

(defmacro gb-iterate-whole
  [gb i-sym c-sym & body]
  ~(do (def {:gap-start gap-start
             :gap-stop gap-stop
             :text text
             :gap gap} ,gb)
     
     
     (loop [,i-sym :range [0 gap-start]
            :let [,c-sym (text ,i-sym)]]
       ,;body)
     
     (loop [,i-sym :range [0 (length gap)]
            :let [,c-sym (gap ,i-sym)]]
       ,;body)
     
     (loop [,i-sym :range [gap-stop (length text)]
            :let [,c-sym (text ,i-sym)]]
       ,;body)))

(varfn render
  [gb]
  (def b2 (buffer/new (gb-length gb)))
  
  (gb-iterate-whole
    gb _ c
    (buffer/push-byte b2 c))
  
  (string
    (buffer/slice b2 0 (gb :caret))
    "|"
    (buffer/slice b2 (gb :caret))))

(comment
  (deep= (-> (string->gb "ab|c")
             forward-char!
             render) 
         "abc|")
  #=> true
  
  (deep= (-> (string->gb "ab(c)|")
             commit!
             render)
         "ab|")
  #=> true
  )

(varfn undo
  [gb]
  (if (empty? (gb :actions))
    gb
    (let [action (array/pop (gb :actions))
          {:content c
           :caret-after i-after           
           :caret-before i-before
           :selection-after sel-after           
           :selection-before sel-before
           :kind kind
           :start start
           :stop stop} action]
      
      (case kind
        :insert (-> gb
                    (remove-region* i-before i-after)
                    (put :caret i-before)
                    (put :changed true))
        :remove (-> gb
                    (put :caret start)
                    (insert-string-at-caret c)
                    (put :caret i-before)
                    (put :selection sel-before)
                    (put :changed true))
        (do (update gb :actions array/push action)
          (error (string "Not supported kind: " kind)))))))

(comment
  (-> (string->gb "ab|")
      (insert-char :1)
      (insert-char :2)
      (insert-char :3)
      (|(do (print (render $)) $))
      undo
      undo
      undo
      render)

  (-> (string->gb "abcd|")
      (remove-region! 0 2)
      (|(do (print (render $)) $))
      undo
      render))


