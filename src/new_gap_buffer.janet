(comment
  "Functions other than `commit!` that ends with `!` in this file runs `commit!`.
This means that the function is O(n) where n is the total length of the gap buffer.
The other functions are O(1) or O(n) where n is the length of the gap, which is generally smaller.")

(import spork/test)

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

(varfn update-gap-pos!
  "Commits then puts `(f (gb :gap-start))` into :gap-start & :gap-stop.
Does bounds check as well."
  [gb f]
  (def {:gap-start gap-start
        :gap-stop gap-stop}
    (commit! gb))
  
  (def new-pos (bounds-check gb (f gap-start)))
  
  (-> gb
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

(varfn forward-char!
  [gb]
  (update-gap-pos! gb inc))

(varfn replace-content
  [gb text]
  (-> gb
      (update :gap buffer/clear)
      (update :text (comp |(buffer/push-string $ text) buffer/clear))
      (put :gap-start 0)
      (put :gap-stop 0)))

(varfn insert-char
  [gb k]
  (let [k (case k
            :space (chr " ")
            :grave (chr "`")
            :left-bracket (chr "[")
            :right-bracket (chr "]")
            k)]
    (comment
      (do (buffer/clear selected)
        (if (keyword? k)
          (buffer/push-string text (string k))
          (put text (length text) k)))
      )
    (update gb :gap buffer/push-byte k)))

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
    
    (update gb :gap buffer/push-byte k)))

(comment
  (insert-char @{:text @""
                 :gap-start 0
                 :gap-stop 0
                 :gap @""}
               (first "a"))
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @"a"}
  
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
  (if (empty? (gb :gap))
    (update gb :gap-start |(max 0 (dec $)))
    (update gb :gap buffer/popn 1)))

(comment
  (backspace @{:text @""
               :gap-start 0
               :gap-stop 0
               :gap @""})
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @""}
  
  (backspace @{:text @"a"
               :gap-start 0
               :gap-stop 0
               :gap @""})
  #=> @{:gap-start 0 :gap-stop 0 :text @"a" :gap @""}
  
  (backspace @{:text @"a"
               :gap-start 1
               :gap-stop 1
               :gap @""})
  #=> @{:gap-start 0 :gap-stop 1 :text @"a" :gap @""}
  
  (backspace @{:text @""
               :gap-start 0
               :gap-stop 0
               :gap @"a"})
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @""}
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
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @""} 
  
  (forward-char! @{:text @"a"
                   :gap-start 0
                   :gap-stop 0
                   :gap @""})
  #=> @{:gap-start 1 :gap-stop 1 :text @"a" :gap @""}
  
  (forward-char! @{:text @""
                   :gap-start 0
                   :gap-stop 0
                   :gap @"b"})
  #=> @{:gap-start 1 :gap-stop 1 :text @"b" :gap @""}
  
  (forward-char! @{:text @"012"
                   :gap-start 0
                   :gap-stop 0
                   :gap @"b"})
  #=> @{:gap-start 2 :gap-stop 2 :text @"b012" :gap @""}
  
  (-> @{:text @"012"
        :gap-start 0
        :gap-stop 0
        :gap @"b"}
      delete
      forward-char!
      commit!)
  #=> @{:gap-start 2 :gap-stop 2 :text @"b12" :gap @""}
  
  (-> @{:text @"012"
        :gap-start 0
        :gap-stop 0
        :gap @"b"}
      forward-char!
      delete
      delete
      commit!)
  #=> @{:gap-start 2 :gap-stop 2 :text @"b0" :gap @""}
  )

(varfn backward-char
  [gb]
  (update-gap-pos! gb dec))

(comment
  (backward-char @{:text @"a"
                   :gap-start 1
                   :gap-stop 1
                   :gap @""})  
  #=> @{:gap-start 0 :gap-stop 0 :text @"a" :gap @""}

  (-> @{:text @"ab"
        :gap-start 2
        :gap-stop 2
        :gap @""}
      backward-char
      backward-char)
  #=> @{:gap-start 0 :gap-stop 0 :text @"ab" :gap @""}
  
  (-> @{:text @"ab"
        :gap-start 1
        :gap-stop 1
        :gap @""}
      backward-char
      backward-char)
  #=>  @{:gap-start 0 :gap-stop 0 :text @"ab" :gap @""}
  )

(def space (first " "))
(def newline (first "\n"))

(varfn word-delimiter?
  [c]
  (or (= c space)
      (= c newline)))

## possible functions
## move-cursor-to
## get-current-cursor-pos

(varfn backward-word
  [gb]
  (def {:gap gap
        :gap-start gap-start
        :gap-stop gap-stop
        :text text} gb)
  (if (empty? (gb :gap))
    (do (var i gap-start)
      (while (and (pos? i)
                  (not (word-delimiter? (gb-nth gb i))))
        (-= i 1))
      (-> gb
          (put :gap-start i)
          (put :gap-stop i)))
    (print "FIX BACKWARD-WORD WHEN GAP NOT EMPTY"))
  gb)

(comment
  (backward-word @{:text @"abc"
                   :gap-start 2
                   :gap-stop 2
                   :gap @""})
  
  (let [t "abc def"]
    (backward-word @{:text t
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

(varfn text-iterator3-backward
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
    (yield [(+ gap-start i) (gap i)]))
  
  (loop [i :down-to [(dec gap-start) 0]]
    (yield [i (text i)]))
  
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
          
          
          (loop [,i-sym :range [(max ,start 0) (min ,stop gap-start)]
                 :let [,c-sym (text ,i-sym)]]
            ,;body)
          
          (loop [,i-sym :range [(max (- ,start gap-start) 0) (min (- ,stop gap-start) (length gap))]
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
                       ,i-sym (+ ,i-sym (length gap))]]
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
