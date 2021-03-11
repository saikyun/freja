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
    :caret caret
    :selection selection
    :changed changed
    :changed-nav changed-nav
    :changed-selection changed-selection}]
  @{:text text
    :gap-start gap-start
    :gap-stop gap-stop
    :gap gap
    :caret caret
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
          {:caret caret} gb
          sel-start (min selection caret)
          sel-stop  (max selection caret)
          new-caret-pos (if (< selection caret)
                          selection
                          caret)]
      (print "huh?")
      (pp gb)
      (-> gb
          (put :selection nil)
          (put :gap-start sel-start)
          (put :gap-stop sel-stop)
          (put :caret new-caret-pos)
          (put :changed true)
          commit!))
    gb))

(defn select-keys
  [t ks]
  (def nt (table/new (length ks)))
  (loop [k :in ks
         :let [v (t k)]]
    (put nt k v))
  nt)

(defn remove-chars
  [cs s]
  (reduce
    (fn [s c]
      (string/replace (c->s c) "" s))
    s
    cs))

(defn string->gb
  [s]
  (def gb-text
    ~{:caret     (replace (* ($) "|") ,(fn [pos] [:caret pos]))
      :selection (replace (* ($) "*") ,(fn [pos] [:selection pos]))
      :gap-content (* (/ ($) ,(fn [start] [:content-start start]))
                      "[" 
                      (any (+ :markers
                              (/ (<- :string) ,(fn [v] [:gap v]))))
                      "]"
                      (/ ($) ,(fn [stop] [:content-stop stop])))
      
      :gap-eat (* (/ ($) ,(fn [start]
                            [:gap-start start]))
                  "("
                  (any (+ :markers
                          (/ (<- :string) ,(fn [v] [:gap-eat v]))))
                  ")"
                  (/ ($) ,(fn [stop] [:gap-stop stop])))
      
      :string :w*
      :markers (+ :caret :selection :gap-content)
      :main (any (+ :gap-eat :markers (<- :string)))})
  
  (var i-to-remove 0)
  (def gap-eat @"")
  (def gb @{:gap @""
            :text @""})
  (loop [v :in (peg/match gb-text s)]
    (if (string? v)
      (update gb :text buffer/push-string v)
      (let [[kind i] v]
        (case kind
          :gap (update gb :gap buffer/push-string i)
          
          :gap-eat (do (buffer/push-string gap-eat i)
                     (+= i-to-remove (length i))
                     (update gb :text buffer/push-string i))
          
          :gap-stop (do (put gb :gap-stop (dec (+
                                                 (length gap-eat)
                                                 (- (length (gb :gap)))
                                                 (- i
                                                    i-to-remove))))
                      (++ i-to-remove))
          
          :content-start
          (do (when (not (gb :gap-start))
                (put gb :gap-start (- i i-to-remove))
                (put gb :gap-stop (- i i-to-remove)))
            (++ i-to-remove))
          
          :content-stop
          (++ i-to-remove)
          
          (do
            (put gb kind (- i i-to-remove))
            (++ i-to-remove))))))  
  
  (when (not (gb :caret))
    (put gb :caret (- (+ (length (gb :text))
                         (length (gb :gap)))
                      (- (get gb :gap-stop 0)
                         (get gb :gap-start 0)))))
  
  (when (not (gb :gap-start))
    (put gb :gap-start (gb :caret))
    (put gb :gap-stop  (gb :caret)))
  
  (update gb :gap buffer)
  
  gb)



(comment
  
  ### when removing selection, we commit and adjust the caret position
  (-> (remove-selection!
        @{:text @"" :selection 0 :gap-stop 0 :caret 1 :gap-start 0 :gap @"a"})
      (select-keys [:gap :caret :text]))
  #=> @{:text @"" :caret 0 :gap @""}
  
  (-> (remove-selection!
        @{:text @"" :selection 0 :gap-stop 0 :caret 0 :selection 1 :gap-start 0 :gap @"a"})
      (select-keys [:gap :caret :text]))
  #=>  @{:text @"" :caret 0 :gap @""}
  
  )

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

(varfn move-gap-to-caret!
  [gb]
  (def {:gap-start gap-start
        :gap-stop gap-stop
        :gap gap
        :caret caret} gb) 
  
  (pp (ez-gb gb))
  
  (if (or (< caret (+ gap-start (length gap)))
          (> caret (+ gap-start (length gap))))
    (put-gap-pos! gb caret)
    gb))

(comment
  (ez-gb gb-data)
  )

(varfn insert-at-caret
  [gb c]
  (let [{:caret caret
         :gap-start gap-start
         :gap gap} (move-gap-to-caret! gb)
        gap-i (tracev (- caret gap-start))]
    (-> gb
        (update :gap
                (fn [gap]
                  (let [to-the-right (buffer/slice gap gap-i)]
                    (buffer/popn gap (- (length gap) gap-i))
                    (buffer/push-byte gap c)
                    (buffer/push-string gap to-the-right))))
        (update :caret inc))))

(varfn remove-at-caret
  [gb]
  (let [{:caret caret
         :gap-start gap-start
         :gap gap} (move-gap-to-caret! gb)
        gap-i (tracev (- caret gap-start))]
    

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
  (def {:selection selection} gb)
  
  (let [k (case k
            :space (chr " ")
            :grave (chr "`")
            :left-bracket (chr "[")
            :right-bracket (chr "]")
            (if (keyword? k)
              (first k)
              k))]
    (insert-at-caret gb k))
  
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
  (let [k (case k
            :space (chr " ")
            :grave (chr "`")
            :left-bracket (chr "[")
            :right-bracket (chr "]")
            (if (keyword? k)
              (ascii-upper-chr (first k))
              k))]
    (insert-at-caret gb k))
  
  (-> gb
      remove-selection!
      (put :changed true)))

(varfn backspace
  [gb]
  (def {:selection selection} gb)
  
  (print "bs!" selection)
  
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

(varfn update-caret
  [gb f]
  (update gb :caret |(bounds-check gb (f $))))

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
