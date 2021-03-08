(import ../src/new_gap_buffer :prefix "")
(import spork/test)

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
