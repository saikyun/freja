(import ../src/new_gap_buffer :prefix "")
(import ../src/new_gap_buffer_util :prefix "")
(import spork/test)

### iterators
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


### gb-length
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





### more iterators
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






### removal
(comment
  (defn select-keys
    [t ks]
    (def nt (table/new (length ks)))
    (loop [k :in ks
           :let [v (t k)]]
      (put nt k v))
    nt)
  
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






### gb-nth
(comment
  (= (do (def gb @{:text @"abc"
                   :gap-start 0
                   :gap-stop 0
                   :gap @"123"})
       (gb-nth gb 3))
     (first "a"))
  #=> true
  )

### insert-char
(comment
  (insert-char @{:text @""
                 :gap-start 0
                 :gap-stop 0
                 :gap @""}
               (first "a"))
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @"a" :changed true :changed-x-pos true}
  
  (-> (insert-char @{:text @""
                     :gap-start 0
                     :gap-stop 0
                     :gap @""}
                   (first "a"))
      commit!)
  #=> @{:gap-start 1 :gap-stop 1 :text @"a" :gap @"" :changed true :changed-x-pos true}
  )

### backspace
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

### delete
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

### forward-char!
(comment
  (let [t (math/rng-buffer (math/rng) (* 1000 1000 10))]
    (-> (test/timeit
          (forward-char! @{:text t
                           :gap-start 0
                           :caret 0
                           :gap-stop 0
                           :gap t}))
        (get :gap-start)))
  
  (forward-char! @{:text @""
                   :gap-start 0
                   :caret 0
                   :gap-stop 0
                   :gap @""})
  #=> @{:gap-start 0 :gap-stop 0 :text @"" :gap @"" :changed-nav true :changed-x-pos true}
  
  (forward-char! @{:text @"a"
                   :gap-start 0
                   :caret 0
                   :gap-stop 0
                   :gap @""})
  #=> @{:gap-start 1 :gap-stop 1 :text @"a" :gap @"" :changed-nav true :changed-x-pos true}
  
  (forward-char! @{:text @""
                   :gap-start 0
                   :caret 0
                   :gap-stop 0
                   :gap @"b"})
  #=> @{:gap-start 1 :gap-stop 1 :text @"b" :gap @"" :changed-nav true :changed-x-pos true}
  
  (forward-char! @{:text @"012"
                   :gap-start 0
                   :caret 0
                   :gap-stop 0
                   :gap @"b"})
  #=> @{:gap-start 2 :gap-stop 2 :text @"b012" :gap @"" :changed-nav true :changed-x-pos true}
  
  (-> @{:text @"012"
        :gap-start 0
        :caret 0
        :gap-stop 0
        :gap @"b"}
      delete
      forward-char!
      commit!)
  #=> @{:gap-start 2 :gap-stop 2 :text @"b12" :gap @"" :changed-nav true :changed-x-pos true}
  
  (-> @{:text @"012"
        :gap-start 0
        :caret 0
        :gap-stop 0
        :gap @"b"}
      forward-char!
      delete
      delete
      commit!)
  #=> @{:gap-start 2 :gap-stop 2 :text @"b0" :gap @"" :changed-nav true :changed-x-pos true}
  )

### backward-char!
(comment
  (backward-char! @{:text @"a"
                    :gap-start 1
                    :gap-stop 1
                    :gap @""})  
  #=> @{:gap-start 0 :gap-stop 0 :text @"a" :gap @"" :changed-nav true :changed-x-pos true}
  
  (-> @{:text @"ab"
        :gap-start 2
        :gap-stop 2
        :gap @""}
      backward-char!
      backward-char!)
  #=> @{:gap-start 0 :gap-stop 0 :text @"ab" :gap @"" :changed-nav true :changed-x-pos true}
  
  (-> @{:text @"ab"
        :gap-start 1
        :gap-stop 1
        :gap @""}
      backward-char!
      backward-char!)
  #=>  @{:gap-start 0 :gap-stop 0 :text @"ab" :gap @"" :changed-nav true :changed-x-pos true}
  )


### select-forward-char!
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
                          :gap-start 2
                          :text @"abc"
                          :selection 2
                          :caret 0
                          :gap @""})
  #=> @{:gap-stop 2 :changed-selection true :gap-start 2 :text @"abc" :selection 2 :caret 1 :gap @""}
  
  )


### caret-pos
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
  )

### backward-word!
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

### render


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



### string->gb
(comment
  (-> (string->gb "ab|c")
      forward-char!
      render) 
  #=> "abc|"

  (-> (string->gb "ab(c)|")
      commit!
      render)
  #=> "ab|"
  
  )
