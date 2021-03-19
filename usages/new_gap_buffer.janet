(use jaylib)
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
  (let [gb @{:text @"abc"
             :gap-start 2
             :gap-stop 3
             :gap @"123"}]
    (gb-iterate gb
                0 (gb-length gb)
                i c
                (print "c: " c " - i: " i)))
  
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
  (-> (string->gb "*[a]|")
      delete-selection!
      render)
  #=> "|"
  
  (-> (string->gb "|[a]*")
      delete-selection!
      render)
  #=> "|"
  
  )






### gb-nth
(comment
  (= (let [gb @{:text @"abc"
                :gap-start 0
                :gap-stop 0
                :gap @"123"}]
       (gb-nth gb 3))
     (chr "a"))
  #=> true
  )

### insert-char!
(comment
  (-> (string->gb "|")
      (insert-char! (chr "a")) 
      render)
  #=> "a|"
  
  (-> (string->gb "*a|")
      (insert-char! (chr "b")) 
      render)
  #=> "b|"
  )

### backspace!
(comment
  (-> (string->gb "|")
      backspace!
      render)
  #=> "|"
  
  (-> (string->gb "|a")
      backspace!
      render)
  #=> "|a"
  
  (-> (string->gb "a|")
      backspace!
      render)
  #=> "|"
  
  (-> (string->gb "|[a]")
      backspace!
      render)  
  #=> "|a"
  )

### forward-char
(comment
  (-> (string->gb "|")
      forward-char
      render)
  #=> "|"
  
  (-> (string->gb "|a")
      forward-char
      render)
  #=> "a|"
  
  (-> (string->gb "|[b]")
      forward-char
      render)
  #=> "b|"
  
  (-> (string->gb "|[b]012")
      forward-char
      render)
  #=> "b|012"
  )

### backward-char
(comment
  (-> (string->gb "a|")
      backward-char
      render)
  #=> "|a"
  
  (-> (string->gb "ab|")
      backward-char
      backward-char
      render)
  #=> "|ab"
  
  (-> (string->gb "a|b")
      backward-char
      backward-char
      render)
  #=> "|ab"
  )


### select-forward-char
(comment
  ## making the selection bigger
  (-> (string->gb "|abc")
      select-forward-char
      render)
  #=> "*a|bc"
  
  (-> (string->gb "*a|bc")
      select-forward-char
      render)
  #=> "*ab|c"
  
  
  ## making the selection smaller
  (-> (string->gb "|ab*c")
      select-forward-char
      render) 
  #=> "a|b*c"
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
  (render (string->gb "(a[123])bc"))
  #=> "123bc|"
  
  (let [gb @{:text @"abc"
             :gap-start 0
             :gap-stop 1
             :gap @"123"}]
    (deep= (render gb)
           (-> gb
               commit!
               render)))
  #=> true
  
  
  (render (string->gb "a|bc*"))
  #=> "a|bc*"
  )



### string->gb
(comment
  (-> (string->gb "ab|c")
      forward-char
      render) 
  #=> "abc|"

  (-> (string->gb "ab(c)|")
      commit!
      render)
  #=> "ab|"
  
  )







### cut / copy / paste
(comment
  (get-selection (string->gb "*(a[b]c)|"))
  #=> @"b"
  )



### undo

(comment
  (-> (string->gb "ab|")
      (insert-char! :1)
      (insert-char! :2)
      (insert-char! :3)
      (|(do (print (render $)) $))
      undo!
      undo!
      undo!
      render)
  #=> "ab|"
  
  (-> (string->gb "ab|")
      (insert-char! :1)
      (insert-char! :2)
      (insert-char! :3)
      (|(do (print (render $)) $))
      undo!
      undo!
      (|(do (print (render $)) $))
      undo!
      (|(do (print (render $)) $))
      redo!
      (|(do (print (render $)) $))
      redo!
      (|(do (print (render $)) $))
      redo!
      render)
  
  (-> (string->gb "ab|")
      (insert-char! :1)
      (insert-char! :2)
      (|(do (pp $) $))
      (|(do (print (render $)) $))
      undo!
      undo!
      (|(do (pp $) $))
      (|(do (print (render $)) $))
      redo!
      redo!
      (|(do (pp $) $))
      render)

  (-> (string->gb "abcd|")
      (delete-region! 0 2)
      (|(do (print (render $)) $))
      undo!
      render)

  (-> (string->gb "abcd|")
      (|(do (print (render $)) $))
      (delete-region! 0 2)
      (|(do (print (render $)) $))
      (delete-region! 0 2)
      (|(do (print (render $)) $))
      undo!
      (|(do (print (render $)) $))
      undo!
      (|(do (print (render $)) $))
      redo!
      (|(do (print (render $)) $))
      redo!
      render
      print)
  )


### undo test for every part of the highest level of the api
(comment
  # delete-selection!
  (def s "ab*cd|")
  (-> (string->gb s)
      delete-selection!
      undo!
      render)
  #=> s
  
  (gb-slice (string->gb "abc(d)|") 3 4)
  
  # delete-before-caret!
  (def s "abcd|")  
  (-> (string->gb s)
      delete-before-caret!
      undo!
      render)  
  #=> s
  
  # delete-word-forward!
  (def s "abcd[1|]")
  (-> (string->gb s)
      delete-before-caret!
      undo!
      render)  
  #=> (render (string->gb s))
  ## ^ need to render because the gap is removed from the original `s`
  
  # delete-word-backward!
  (def s "oeau abcd a|")
  (-> (string->gb s)
      delete-word-backward!
      undo!
      render)
  #=> s
  
  (def s "oeau abcd[1|]")
  (-> (string->gb s)
      delete-word-backward!
      undo!
      render)    
  #=> (render (string->gb s))
  ## ^ need to render because the gap is removed from the original `s`
  
  # backspace!
  (def s "hej lul|")  
  (-> (string->gb s)
      backspace!
      undo!
      render)  
  #=> s
  
  # insert-char!
  (def s "hello ther|")  
  (-> (string->gb s)
      (insert-char! (chr "e"))
      undo!
      render)  
  #=> s
  
  # insert-char-upper!
  (def s "i said HELLO THER|")  
  (-> (string->gb s)
      (insert-char-upper! (chr "e"))
      undo!
      render)    
  #=> s
  
  
  # cut!
  (def s "i said [*HELLO THER|]")
  (-> (string->gb s)
      cut! 
      undo!
      render)
  #=> (render (string->gb s))
  
  # paste!

  ## this only works with a jaylib window
  ## so I have it disabled by default
  
  # (init-window 1200 700
  #              "Textfield")
  
  # (def s "i said [*HELLO THER|]")
  # (def gb (-> (string->gb s)
  #             cut!))
  # (def s2 (render gb))
  # (deep= (-> gb
  #            paste!
  #            undo!
  #            render)
  #        s2)
  
  # (close-window)
  )





### render
(comment
  (deep= (-> (string->gb "ab|c")
             forward-char
             render) 
         "abc|")
  #=> true
  
  (deep= (-> (string->gb "ab(c)|")
             commit!
             render)
         "ab|")
  #=> true
  )
