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

(comment

  )

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

(comment
  (let [gb @{:text @"abc def"
             :gap-start 0
             :gap-stop 0
             :gap @""}
        f (fiber/new (fn [] (index-char-start gb 3)))
        s @""]
    (loop [[i c] :iterate (resume f)]
      (buffer/push-byte s c))
    s)
  
  (let [gb @{:text @"abc def"
             :gap-start 0
             :gap-stop 0
             :gap @""}
        f (fiber/new (fn [] (index-char-backward-start gb 3)))
        s @""]
    (loop [[i c] :iterate (resume f)]
      (buffer/push-byte s c))
    s)
  
  
  

  (let [gb @{:gap-stop 4 :changed-nav false :changed-selection false :gap-start 0 :text @"hellu aoeu" :gap @"abca" :changed false}
        a @[]]
    (gb-iterate gb
                0 100
                i c
                (array/push a i))
    a)
  #=>  @[0 1 2 3 4 5 6 7 8 9]
  
  
  
  (let [s1 (let [gb @{:text @"abc def"
                      :gap-start 3
                      :gap-stop 5
                      :gap @"123"}
                 s @""]
             (gb-iterate gb
                         0 100
                         i c
                         (buffer/push-byte s c))
             s)
        s2 (let [gb @{:text @"abc def"
                      :gap-start 3
                      :gap-stop 5
                      :gap @"123"}
                 f (fiber/new (fn [] (index-char-start gb 0)))
                 s @""]
             (loop [[i c] :iterate (resume f)]
               (buffer/push-byte s c))
             s)]
    (print s1)
    (print s2)
    (deep= s1 s2))
  #=> true
  
  (let [s1 (let [gb @{:text @"abc def"
                      :gap-start 3
                      :gap-stop 5
                      :gap @"123"}
                 s @""]
             (gb-iterate gb
                         5 100
                         i c
                         (buffer/push-byte s c))
             s)
        s2 (let [gb @{:text @"abc def"
                      :gap-start 3
                      :gap-stop 5
                      :gap @"123"}
                 f (fiber/new (fn [] (index-char-start gb 5)))
                 s @""]
             (loop [[i c] :iterate (resume f)]
               (buffer/push-byte s c))
             s)]
    (print s1)
    (print s2)
    (deep= s1 s2))
  #=> true
  )

(defn gb-length
  [{:text text
    :gap-start gap-start
    :gap-stop gap-stop
    :gap gap}]
  (- (+ (length text)
        (length gap))
     (- gap-stop gap-start)))

(comment
  (gb-length {:text @""
              :gap-start 0
              :gap-stop 0
              :gap @""})
  #=> 0
  
  (gb-length {:text @"a"
              :gap-start 0
              :gap-stop 0
              :gap @""})
  #=> 1
  
  (gb-length {:text @"a"
              :gap-start 0
              :gap-stop 0
              :gap @"b"}) 
  #=> 2
  
  (gb-length {:text @"a"
              :gap-start 1
              :gap-stop 1
              :gap @"b"}) 
  #=> 2
  
  (gb-length {:text @"ab"
              :gap-start 1
              :gap-stop 2
              :gap @""})
  #=> 1
  
  (gb-length {:text @"abc"
              :gap-start 1
              :gap-stop 2
              :gap @"123"})
  #=> 5
  )

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

(comment
  (let [gb @{:text @"abc def"
             :gap-start 3
             :gap-stop 5
             :gap @"123"}
        f (fiber/new (fn [] (index-char-backward-start gb 10)))
        s @""]
    (loop [[i c] :iterate (resume f)]
      (buffer/push-byte s c))
    s)
  #=> @"fe321cba"
  

  (let [gb @{:text @"abc def"
             :gap-start 3
             :gap-stop 5
             :gap @"123"}
        f (fiber/new (fn [] (index-char-backward-start gb 3)))
        s @""]
    (loop [[i c] :iterate (resume f)]
      (buffer/push-byte s c))
    s)
  #=> @"1cba"
  
  (let [s1 (let [gb @{:text @"abc def"
                      :gap-start 3
                      :gap-stop 5
                      :gap @"123"}
                 s @""]
             (gb-iterate gb
                         0 100
                         i c
                         (buffer/push-byte s c))
             s)
        s2 (let [gb @{:text @"abc def"
                      :gap-start 3
                      :gap-stop 5
                      :gap @"123"}
                 f (fiber/new (fn [] (index-char-backward-start gb 10)))
                 s @""]
             (loop [[i c] :iterate (resume f)]
               (buffer/push-byte s c))
             s)]
    (print s1)
    (print s2)
    (deep= (string s1) (string/reverse s2)))
  #=> true
  )

(varfn remove-selection!
  [gb]
  (def {:selection selection
        :gap-start gap-start} (commit! gb))
  (if selection
    (let [sel-start (min selection gap-start)
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

(comment
  (= (do (def gb @{:text @"abc"
                   :gap-start 0
                   :gap-stop 0
                   :gap @"123"})
       (gb-nth gb 3))
     (first "a"))
  #=> true
  
  )

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

(comment
  (insert-char @{:text @""
                 :gap-start 0
                 :gap-stop 0
                 :gap @""}
               (first "a"))
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @"a" :changed true}
  
  (-> (insert-char @{:text @""
                     :gap-start 0
                     :gap-stop 0
                     :gap @""}
                   (first "a"))
      commit!)
  @{:gap-start 1 :gap-stop 1 :text @"a" :gap @""}
  )

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

(comment
  (backspace @{:text @""
               :gap-start 0
               :gap-stop 0
               :gap @""})
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @"" :changed true}
  
  (backspace @{:text @"a"
               :gap-start 0
               :gap-stop 0
               :gap @""})
  #=> @{:gap-start 0 :gap-stop 0 :text @"a" :gap @"" :changed true}
  
  (backspace @{:text @"a"
               :gap-start 1
               :gap-stop 1
               :gap @""})
  #=> @{:gap-start 0 :gap-stop 1 :text @"a" :gap @"" :changed true}
  
  (backspace @{:text @""
               :gap-start 0
               :gap-stop 0
               :gap @"a"})
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @"" :changed true}
  )

(varfn delete
  [gb]
  (update gb :gap-stop |(min (length (gb :text)) (inc $))))

(comment
  (delete @{:text @""
            :gap-start 0
            :gap-stop 0
            :gap @""})
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @""}
  
  (delete @{:text @"a"
            :gap-start 0
            :gap-stop 0
            :gap @""})
  #=> @{:gap-start 0 :gap-stop 1 :text @"a" :gap @""}
  
  (delete @{:text @"a"
            :gap-start 1
            :gap-stop 1
            :gap @""})
  #=> @{:gap-start 1 :gap-stop 1 :text @"a" :gap @""}
  
  (delete @{:text @""
            :gap-start 0
            :gap-stop 0
            :gap @"a"})
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @"a"}
  
  
  (-> @{:text @"abcd"
        :gap-start 0
        :gap-stop 0
        :gap @""}
      delete
      delete
      delete
      delete
      commit!)
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @""}
  )

(varfn forward-char!
  [gb]
  (-> gb
      (update-gap-pos! inc)
      (put :changed-x-pos true)))

(comment
  (let [t (math/rng-buffer (math/rng) (* 1000 1000 10))]
    (-> (test/timeit
          (forward-char! @{:text t
                           :gap-start 0
                           :gap-stop 0
                           :gap t}))
        (get :gap-start)))
  
  (forward-char! @{:text @""
                   :gap-start 0
                   :gap-stop 0
                   :gap @""})
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @"" :changed-nav true}
  
  (forward-char! @{:text @"a"
                   :gap-start 0
                   :gap-stop 0
                   :gap @""})
  #=> @{:gap-start 1 :gap-stop 1 :text @"a" :gap @"" :changed-nav true}
  
  (forward-char! @{:text @""
                   :gap-start 0
                   :gap-stop 0
                   :gap @"b"})
  #=> @{:gap-start 1 :gap-stop 1 :text @"b" :gap @"" :changed-nav true}
  
  (forward-char! @{:text @"012"
                   :gap-start 0
                   :gap-stop 0
                   :gap @"b"})
  #=> @{:gap-start 2 :gap-stop 2 :text @"b012" :gap @"" :changed-nav true}
  
  (-> @{:text @"012"
        :gap-start 0
        :gap-stop 0
        :gap @"b"}
      delete
      forward-char!
      commit!)
  #=> @{:gap-start 2 :gap-stop 2 :text @"b12" :gap @"" :changed-nav true}
  
  (-> @{:text @"012"
        :gap-start 0
        :gap-stop 0
        :gap @"b"}
      forward-char!
      delete
      delete
      commit!)
  #=> @{:gap-start 2 :gap-stop 2 :text @"b0" :gap @"" :changed-nav true}
  )

(varfn backward-char!
  [gb]
  (-> gb
      (update-gap-pos! dec)
      (put :changed-x-pos true)))

(comment
  (backward-char! @{:text @"a"
                    :gap-start 1
                    :gap-stop 1
                    :gap @""})  
  #=> @{:gap-start 0 :gap-stop 0 :text @"a" :gap @"" :changed-nav true}
  
  (-> @{:text @"ab"
        :gap-start 2
        :gap-stop 2
        :gap @""}
      backward-char!
      backward-char!)
  #=> @{:gap-start 0 :gap-stop 0 :text @"ab" :gap @"" :changed-nav true}
  
  (-> @{:text @"ab"
        :gap-start 1
        :gap-stop 1
        :gap @""}
      backward-char!
      backward-char!)
  #=>  @{:gap-start 0 :gap-stop 0 :text @"ab" :gap @"" :changed-nav true}
  )

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

(comment
  ## making the selection bigger
  (select-forward-char! @{:text @"a"
                          :gap-start 1
                          :gap-stop 1
                          :gap @""})
  #=> @{:gap-stop 1 :changed-selection true :gap-start 1 :text @"a" :selection 2 :gap @""}
  
  (select-forward-char! @{:gap-stop 1 
                          :changed-selection true
                          :gap-start 1
                          :text @"a"
                          :selection 2
                          :gap @""})
  #=> @{:gap-stop 1 :changed-selection true :gap-start 1 :text @"a" :selection 3 :gap @""}
  
  
  ## making the selection smaller
  (select-forward-char! @{:gap-stop 2
                          :changed-selection true
                          :gap-start 2
                          :text @"abc"
                          :selection 0
                          :gap @""})
  #=> @{:gap-stop 2 :changed-selection true :gap-start 2 :text @"abc" :selection 1 :gap @""}
  
  )

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

(comment
  (caret-pos @{:text @"abc def"
               :gap-start 3
               :gap-stop 5
               :gap @"123"})
  #=> 6

  (caret-pos @{:text @"abc def"
               :gap-start 5
               :gap-stop 5
               :gap @""})
  #=> 5
  
  #(fiber/status (fiber/new (fn [] (index-char-backward-start gb-data 10))))
  )

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

(comment
  #(caret-pos gb-data)
  )

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

(comment
  (backward-word! @{:text @"abc"
                    :gap-start 2
                    :gap-stop 2
                    :gap @""})
  
  (let [t "abc def"]
    (backward-word! @{:text t
                      :gap-start (length t)
                      :gap-stop (length t)
                      :gap @""}))
  )

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

(comment
  (let [gb @{:text @"abc"
             :gap-start 1
             :gap-stop 3
             :gap @"123"}
        f (fiber/new (fn [] (index-char-backward gb)))]
    (reduce (fn [a b]
              (and a b))
            true (seq [[i c] :iterate (resume f)]
                   (= (gb-nth gb i) c))))
  #=> true
  
  (let [gb @{:text @"abc def"
             :gap-start 7
             :gap-stop 7
             :gap @""}
        f (fiber/new (fn [] (index-char-backward gb)))]
    (var res nil)
    (loop [[i c] :iterate (resume f)
           :until (word-delimiter? c)]
      (set res i))
    res)
  
  
  (def f (fiber/new (fn [] (text-iterator3 @{:text @"abc"
                                             :gap-start 1
                                             :gap-stop 1
                                             :gap @""})))) 
  
  (seq [c :iterate (resume f)]
    c)
  
  (let [it (text-iterator @{:text @"abc"
                            :gap-start 1
                            :gap-stop 1
                            :gap @""})]
    (string/from-bytes ;(seq [c :iterate (it)]
                          c)))
  #=> "abc"
  
  (let [it (text-iterator @{:text @"abc"
                            :gap-start 0
                            :gap-stop 0
                            :gap @"123"})]
    (string/from-bytes ;(seq [c :iterate (it)]
                          c)))
  
  (let [it (text-iterator @{:text @"abc"
                            :gap-start 1
                            :gap-stop 1
                            :gap @"123"})]
    (string/from-bytes ;(seq [c :iterate (it)]
                          c)))
  #=> "a123bc"
  
  (let [it (text-iterator @{:text @"abc"
                            :gap-start 0
                            :gap-stop 1
                            :gap @"123"})]
    (string/from-bytes ;(seq [c :iterate (it)]
                          c)))
  #=> "123bc"
  
  )

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

(comment
  (gb-iterate @{:gap-start 0 :gap-stop 0 :text @"abcdefuheorlcauhcreloahurclehoalruhecrolahurclehoarlcuhecroahucrleoahcrluheolarh" :gap @""}
              0 100
              i c
              (print i)
              (return stop-gb-iterate))
  )

(def rng (math/rng))

(varfn random-gap-buffer
  [max-len]
  (let [text-len (math/rng-int rng max-len)
        gap-len (math/rng-int rng max-len)
        
        gstart (math/rng-int rng (inc text-len))        
        gstop (+ gstart
                 (math/rng-int rng (- (inc text-len) gstart)))        
        
        gb @{:text (math/rng-buffer rng text-len)
             :gap-start gstart
             :gap-stop gstop
             :gap (math/rng-buffer rng gap-len)}]
    gb))

(comment
  (gb-iterate-whole @{:text @"abc"
                      :gap-start 2
                      :gap-stop 3
                      :gap @"123"}
                    i c
                    (print "c: " c " - i: " i))
  
  (let [gb @{:text @"abcefg"
             :gap-start 0
             :gap-stop 1
             :gap @"hello"}
        b @""]
    (gb-iterate gb
                4
                100
                i c
                (print "c: " c " - i: " i)
                (buffer/push-byte b c))
    (print b))
  
  
  (let [gb (random-gap-buffer 1000)
        b @""]
    (gb-iterate gb
                0
                (gb-length gb)
                i c
                (buffer/push-byte b c))
    (deep= b
           ((commit! gb) :text)))
  #=> true
  
  (reduce |(and $0 $1)
          true
          (seq [_ :range [0 1000]]
            (let [gb (random-gap-buffer 1000)
                  start (math/rng-int rng (gb-length gb))
                  stop (+ start
                          (math/rng-int rng (- (gb-length gb) start)))
                  b @""]
              (gb-iterate gb
                          start
                          stop
                          i c
                          (buffer/push-byte b c))
              (deep= b
                     (buffer/slice ((commit! gb) :text) start stop)))))
  #=> true
  )

(varfn render
  [gb]
  (def b2 (buffer/new (gb-length gb)))
  
  (gb-iterate-whole
    gb _ c
    (buffer/push-byte b2 c))
  
  b2)

(comment
  (render @{:text @"abc"
            :gap-start 0
            :gap-stop 1
            :gap @"123"})
  #=> @"123bc"
  
  (let [gb @{:text @"abc"
             :gap-start 0
             :gap-stop 1
             :gap @"123"}]
    (deep= (render gb)      ## need to use deep= when comparing buffers
           (-> gb
               commit!
               render)))
  #=> true
  )

(comment
  (def rng (math/rng))
  (def size 1000000000) #1gb
  (def size 1000000) #1mb
  (def stuff @{:text @""
               :gap-start 0
               :gap-stop 0
               :gap @""})
  (do (put stuff :text (math/rng-buffer rng size))
    (put stuff :gap (math/rng-buffer rng size))
    :ok)
  
  (insert-char stuff (first "a"))
  (backspace stuff)
  (forward-char! stuff)
  
  (do (test/timeit (commit! stuff))
    :ok)
  
  (do (test/timeit (freeze (stuff :text)))
    :ok)
  
  
  (do (def b2 (buffer/new (* size 2)))
    (test/timeit
      (let [it (text-iterator stuff)]
        (loop [c :iterate (it)]
          (buffer/push-byte b2 c))))) 
  
  (do (def v (math/rng-buffer (math/rng) 1))
    (test/timeit (freeze v))
    :ok)
  
  (do (def v (math/rng-buffer (math/rng) 1000000))
    (test/timeit (freeze v))
    :ok)
  
  (do (test/timeit (do (buffer
                         (stuff :text)
                         (string/reverse (stuff :gap)))))
    :ok)
  
  (commit! @{:text @"aoeu"
             :gap-start 1
             :gap-stop 4
             :gap @"123"})
  
  (test-blit {:text @"aoeu"
              :gap-start 1
              :gap-stop 1
              :gap @"123"})
  #=> @"a123oeu"
  )

















(def rng (math/rng))
#(def size (* 0.25 1000000000)) #1gb
(def size 10000000) #10mb
(def size 100) #100b
(def stuff @{:text @""
             :gap-start 0
             :gap-stop 0
             :gap @""})
(do (put stuff :text (math/rng-buffer rng size))
  (put stuff :gap (math/rng-buffer rng size))
  :ok)

(do (def b2 (buffer/new (* size 2)))
  (def c (first "a"))
  (print "base stuff")
  (test/timeit (loop [i :range [0 (* size 2)]]
                 (buffer/push-byte b2 c))))

(print (= (do (def b2 (buffer/new (* size 2)))
            (print "generator")
            (test/timeit
              (let [f (fiber/new (fn [] (text-iterator3 stuff)))] 
                (loop [c :iterate (resume f)]
                  (buffer/push-byte b2 c))))
            (freeze b2))
          
          (do (def b2 (buffer/new (* size 2)))
            (print "cond")
            (test/timeit
              (let [it (text-iterator stuff)]
                (loop [c :iterate (it)]
                  (buffer/push-byte b2 c))))
            (freeze b2))
          
          (do (def b2 (buffer/new (* size 2)))
            (print "state machine")
            (test/timeit
              (let [it (text-iterator2 stuff)]
                (loop [c :iterate (it)]
                  (buffer/push-byte b2 c))))
            (freeze b2))



          (do (def b2 (buffer/new (* size 2)))
            (print "macro")
            (test/timeit
              (gb-iterate-whole stuff _ c (buffer/push-byte b2 c)))
            
            (freeze b2))
          
          (do (def b2 (buffer/new (* size 2)))
            (print "hard coded")
            (test/timeit
              (do (def {:gap-start gap-start
                        :gap-stop gap-stop
                        :text text
                        :gap gap} stuff)
                
                (loop [c :in (buffer/slice text 0 gap-start)]
                  (buffer/push-byte b2 c))
                
                (loop [c :in gap]
                  (buffer/push-byte b2 c))
                
                
                (loop [c :in (buffer/slice text gap-stop (length text))]
                  (buffer/push-byte b2 c))))
            
            (freeze b2))))
