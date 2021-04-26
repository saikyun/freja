(defmacro line-number
  []
  (def [l c] (tuple/sourcemap (dyn :macro-form ())))
  l)

(defn add []
  (def a 10)
  (+ a a))

#=> turns into

(defn add []
  (def debug/locals @{})
  (yield {:form '(def a 10)
          :line (line-number)})
  (def a 10)
  (put debug/locals 'a @{:value a})
  (yield {:locals debug/locals
          :res a})
  (yield {:form '(+ a a)
          :line (line-number)})
  {:locals debug/locals
   :res (+ a a)})

(defn run-stepped-function
  [f]
  
  (def func-form '(defn add []
                    (def a 10)
                    (+ a a)))
  
  (def fib (fiber/new f))
  (fiber/setenv fib (curenv))
  (var last-res nil)
  (var last-locals nil)
  (var i 0)
  
  (print "Entering stepping.")
  
  (while (not= :dead (fiber/status fib))
    (var {:line line :res res :form form :locals locals} (resume fib last-res))
    
    (set last-res res)
    
    (when form
      (print "Step " (++ i))
      
      (print "(n) -- go to next step.")    
      (print "d/r -- last result.")              
      (print "d/l -- locals")
      (print "d/f -- whole function")
      
      (print "Last form - line " line)
      (pp form))
    
    (when locals
      (set last-locals locals))
    
    (when res
      (repl nil nil (merge-into (make-env)
                                (or last-locals @{})
                                @{'d/r @{:value res}
                                  'd/l @{:value last-locals}
                                  'd/f @{:value func-form}
                                  'n @{:value quit}}))))
  
  (print "Exiting stepping.")

  last-res)

(run-stepped-function add)
