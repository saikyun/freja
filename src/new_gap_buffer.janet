(comment
  "Functions other than `commit!` that ends with `!` in this file runs `commit!`.
This means that the function is O(n) where n is the total length of the gap buffer.
The other functions are O(1) or O(n) where n is the length of the gap, which is generally smaller.")

(import spork/test)

(varfn ez-gb
  [{:text text
    :gap-start gap-start
    :gap-stop gap-stop
    :gap gap
    :selection selection
    :changed changed
    :changed-nav changed-nav
    :changed-selection changed-selection}]
  @{:text text
    :gap-start gap-start
    :gap-stop gap-stop
    :gap gap
    :selection selection
    :changed changed
    :changed-nav changed-nav
    :changed-selection changed-selection})

(defmacro gb-iterate
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

(varfn iterate-backwards
  "Like `text-iterator`, implemented as a generator."
  [gb]
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :text text
        :gap gap} gb)
  
  (loop [i :down-to [(dec (length text)) gap-stop]]
    (yield (text i)))    
  
  (loop [c :in (string/reverse gap)]
    (yield c))    
  
  (loop [i :down-to [(dec gap-start) 0]]
    (yield (text i)))
  
  nil)

(varfn index-char-backward
  "Each time called, signals the next index and character in a gap buffer."
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
Each time called, signals the next index and character in a gap buffer.
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
  "GB iterator implemented as a generator."
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

(defn gb-length
  [{:text text
    :gap-start gap-start
    :gap-stop gap-stop
    :gap gap}]
  (- (+ (length text)
        (length gap))
     (- gap-stop gap-start)))

(defn fresh?
  [{:gap-start gap-start
    :gap-stop gap-stop
    :gap gap}]
  (and (empty? gap)
       (= gap-start gap-stop)))

(varfn commit!
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
  [c]
  (-> (buffer/new 1)
      (buffer/push-byte c)))

(varfn remove-selection!
  [gb]
  (def {:selection selection} gb)
  (if selection
    (let [gb (commit! gb)
          {:gap-start gap-start} gb
          sel-start (min selection gap-start)
          sel-stop  (max selection gap-start)]
      (-> gb
          (put :selection nil)
          (put :gap-start sel-start)
          (put :gap-stop sel-stop)
          (put :changed true)
          commit!))
    gb))

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

(defn bounds-check
  "Returns `i` that is greather than or equal to 0, and lesser than the length of the gap buffer."
  [gb i]
  (min (gb-length gb) (max 0 i)))

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

(varfn insert-char
  [gb k]
  (def {:selection selection} gb)
  
  (let [k (case k
            :space (chr " ")
            :grave (chr "`")
            :left-bracket (chr "[")
            :right-bracket (chr "]")
            (if (keyword? k)
              (first k)
              k))]
    (update gb :gap buffer/push-byte k))
  
  (-> gb
      remove-selection!
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
  (let [k (case k
            :space (chr " ")
            :grave (chr "`")
            :left-bracket (chr "[")
            :right-bracket (chr "]")
            (if (keyword? k)
              (ascii-upper-chr (first k))
              k))]
    
    (update gb :gap buffer/push-byte k))
  
  (-> gb
      remove-selection!
      (put :changed true)))

(varfn backspace
  [gb]
  (def {:selection selection} gb)
  
  (print "bs!" selection)
  
  (cond selection
    (remove-selection! gb)
    
    (empty? (gb :gap))
    (update gb :gap-start |(max 0 (dec $)))
    
    # else
    (update gb :gap buffer/popn 1))
  (put gb :changed true))

(varfn delete
  [gb]
  (update gb :gap-stop |(min (length (gb :text)) (inc $))))

(varfn forward-char!
  [gb]
  (-> gb
      (update-gap-pos! inc)
      (put :changed-x-pos true)))

(varfn backward-char!
  [gb]
  (-> gb
      (update-gap-pos! dec)
      (put :changed-x-pos true)))

(varfn select-forward-char!
  [gb]
  (def {:selection selection
        :gap-start gap-start}
    (commit! gb))
  
  (when (not selection)
    (put gb :selection gap-start))
  
  (-> gb
      (update :selection inc)
      (put :changed-selection true)))

(varfn select-backward-char!
  [gb]
  (def {:selection selection
        :gap-start gap-start}
    (commit! gb))
  
  (when (not selection)
    (put gb :selection gap-start))
  
  (-> gb
      (update :selection dec)
      (put :changed-selection true)))

(def space (first " "))
(def newline (first "\n"))

(varfn word-delimiter?
  [c]
  (or (= c space)
      (= c newline)))

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

(varfn forward-word!
  [gb]
  (def {:gap gap
        :gap-start gap-start
        :gap-stop gap-stop
        :text text} gb)
  
  (var target-i (caret-pos gb))
  
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
  
  (-> gb
      deselect
      (put-gap-pos! target-i)
      (put :changed-x-pos true)))

(varfn backward-word!
  [gb]
  (def {:gap gap
        :gap-start gap-start
        :gap-stop gap-stop
        :text text} gb)
  
  ## the caret pos is between two chars
  # so if we want to go to the char to the left, we decrease the caret by one
  (var target-i (dec (caret-pos gb)))
  
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
  
  (-> gb
      deselect
      (put-gap-pos! target-i)
      (put :changed-x-pos true)))

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
  
  b2)

