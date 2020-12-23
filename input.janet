(use ./build/jaylib)
(import ./text_api :prefix "")
(import ./text_rendering :prefix "")
(import ./highlight :prefix "")

(varfn new-mouse-data
  []
  @{:just-down                  nil
    :just-double-clicked        nil
    :just-triple-clicked        nil
    :recently-double-clicked    nil
    :recently-triple-clicked    nil
    :down-pos                   nil
    :down-time                  nil
    :down-time2                 nil
    :up-pos                     nil
    :selected-pos               nil
    :last-text-pos              nil})

(varfn reset-blink
  [props]
  (set (props :blink) 0))

(varfn eval-it
  [data code]
  (print "Eval! " code)
  (try (do (fiber/setenv (fiber/current) (data :top-env))
           (put data :latest-res (string (eval-string code))))
       ([err fib]
        (put data :latest-res (string "Error: " err))))
  (pp (data :latest-res)))

(varfn handle-keyboard
  [data]
  (def {:text-data props} data)
  (var k (get-key-pressed))
  
  (while (not= 0 k)
    (reset-blink props)
    
    (print "key pressed " k)
    
    (cond (or (key-down? :left-shift)
            (key-down? :right-shift))
          (insert-char-upper props k)
          
          (insert-char props k))
    
    (set k (get-key-pressed)))
  
  (when (and (key-pressed? :q)
          (or (key-down? :left-control)
            (key-down? :right-control)))
    (put data :quit true))  
  
  (when (key-pressed? :home)
    (reset-blink props)
    
    (if (or (key-down? :left-shift)
          (key-down? :right-shift))
      (select-until-beginning props)
      (move-to-beginning props)))  
  
  (when (key-pressed? :end)
    (reset-blink props)    
    
    (if (or (key-down? :left-shift)
          (key-down? :right-shift))
      (select-until-end props)
      (move-to-end props)))  
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :.))
    (reset-blink props)
    
    (paste props))  
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :a))
    (select-all props))  
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :i))
    (copy props))  
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :b))
    (reset-blink props)
    
    (cut props))
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :e))
    (eval-it data (last (peg/match sexp-grammar "(* 5 5) (+ 1 1)")))) 
  
  (when (key-pressed? :backspace)
    (reset-blink props)
    
    (cond (or (key-down? :left-alt)
            (key-down? :right-alt))
          (delete-word-before props)
          
          (backspace props)))  
  
  (when (key-pressed? :delete)
    (reset-blink props)

    (cond (or (key-down? :left-alt)
            (key-down? :right-alt))
          (delete-word-after props)
          
          (forward-delete props)))  
  
  (when (key-pressed? :left)
    (reset-blink props)

    (cond
      ## select whole words
      (and (or (key-down? :left-alt)
             (key-down? :right-alt))
        (or (key-down? :left-shift)
          (key-down? :right-shift)))
      (select-word-before props)
      
      (or (key-down? :left-alt)
        (key-down? :right-alt)) 
      (move-word-before props)
      
      (or (key-down? :left-shift)
        (key-down? :right-shift))
      (select-char-before props)
      
      (move-char-before props)))  
  
  (when (key-pressed? :right)
    (reset-blink props)
    
    (cond 
      (and (or (key-down? :left-alt)
             (key-down? :right-alt))
        (or (key-down? :left-shift)
          (key-down? :right-shift)))
      (select-word-after props)
      
      (or (key-down? :left-alt)
        (key-down? :right-alt))
      (move-word-after props)
      
      (or (key-down? :left-shift)
        (key-down? :right-shift))
      (select-char-after props)
      
      (move-char-after props)))  
  
  (when (key-pressed? :enter)
    (reset-blink props)
    
    (cond
      (or (key-down? :left-control)
        (key-down? :right-control))    
      (do (def code (string
                      (props :text)
                      (props :selected)
                      (string/reverse (props :after))))
          (eval-it data code))
      
      (insert-char props (first "\n")))))

(varfn handle-mouse
  [mouse-data text-data]
  (def [x y] (get-mouse-position))
  
  (put mouse-data :just-double-clicked false)
  (put mouse-data :just-triple-clicked false)
  
  (def both (content text-data))  
  
  (when (mouse-button-released? 0)
    (put mouse-data :last-text-pos nil)
    (put mouse-data :just-down nil)
    (put mouse-data :recently-double-clicked nil)
    (put mouse-data :recently-triple-clicked nil)
    (put mouse-data :up-pos [x y])
    
    (put mouse-data :selected-pos [(get-pos-in-text text-data (first (mouse-data :down-pos)))
                                   (get-pos-in-text text-data x)]))
  
  (when (mouse-button-pressed? 0)
    (when (and (mouse-data :down-time2)
            (> 0.4 (- (get-time) (mouse-data :down-time2))))
      (put mouse-data :just-triple-clicked true)
      (put mouse-data :recently-triple-clicked true))
    
    (when (and (mouse-data :down-time)
            (> 0.25 (- (get-time) (mouse-data :down-time))))
      (put mouse-data :just-double-clicked true)
      (put mouse-data :recently-double-clicked true)
      (put mouse-data :down-time2 (get-time))))
  
  (cond (mouse-data :just-triple-clicked) 
        (select-all text-data)
        
        (and (mouse-data :just-double-clicked)
          (not (key-down? :left-shift))
          (not (key-down? :right-shift)))
        (select-surrounding-word text-data)
        
        (or (mouse-data :recently-double-clicked)
          (mouse-data :recently-triple-clicked)) nil # don't start selecting until mouse is released again
        
        (mouse-button-down? 0)
        (do (when (nil? (mouse-data :last-text-pos))
              (put mouse-data :last-text-pos (length (text-data :text))))
            
            (put mouse-data :down-time (get-time))
            (if (= nil (mouse-data :just-down))
              (do (put mouse-data :just-down true)
                  (put mouse-data :down-pos [x y]))
              (put mouse-data :just-down false))
            
            (put mouse-data :selected-pos [(get-pos-in-text text-data (first (mouse-data :down-pos)))
                                           (get-pos-in-text text-data x)])
            
            (var moved-caret false)
            
            (let [[start end] (mouse-data :selected-pos)
                  start (if (or (key-down? :left-shift)
                              (key-down? :right-shift))
                          (mouse-data :last-text-pos)
                          start)]
              (select-region text-data start end)))))
