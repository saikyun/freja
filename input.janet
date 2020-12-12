(use ./build/jaylib)
(import ./text_api :prefix "")
(import ./text_rendering :prefix "")

(defn new-mouse-data
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

(defn handle-keyboard
  [data]
  (def {:text-data text-data} data)
  (var k (get-key-pressed))
  
  (while (not= 0 k)
    (insert-char text-data k)
    (set k (get-key-pressed)))  
  
  (when (and (key-pressed? :q)
          (or (key-down? :left-control)
            (key-down? :right-control)))
    (put data :quit true))  
  
  (when (key-pressed? :home)
    (if (or (key-down? :left-shift)
          (key-down? :right-shift))
      (select-until-beginning text-data)
      (move-to-beginning text-data)))  
  
  (when (key-pressed? :end)
    (if (or (key-down? :left-shift)
          (key-down? :right-shift))
      (select-until-end text-data)
      (move-to-end text-data)))  
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :.))
    (paste text-data))  
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :a))
    (select-all text-data))  
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :i))
    (copy text-data))  
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :b))
    (cut text-data))  
  
  (when (key-pressed? :backspace)
    (cond (or (key-down? :left-alt)
            (key-down? :right-alt))
          (delete-word-before text-data)
          
          (backspace text-data)))  
  
  (when (key-pressed? :delete)
    (cond (or (key-down? :left-alt)
            (key-down? :right-alt))
          (delete-word-after text-data)
          
          (forward-delete text-data)))  
  
  (when (key-pressed? :left)
    (cond
      ## select whole words
      (and (or (key-down? :left-alt)
             (key-down? :right-alt))
        (or (key-down? :left-shift)
          (key-down? :right-shift)))
      (select-word-before text-data)
      
      (or (key-down? :left-alt)
        (key-down? :right-alt)) 
      (move-word-before text-data)
      
      (or (key-down? :left-shift)
        (key-down? :right-shift))
      (select-char-before text-data)
      
      (move-char-before text-data)))  
  
  (when (key-pressed? :right)
    (cond 
      (and (or (key-down? :left-alt)
             (key-down? :right-alt))
        (or (key-down? :left-shift)
          (key-down? :right-shift)))
      (select-word-after text-data)
      
      (or (key-down? :left-alt)
        (key-down? :right-alt))
      (move-word-after text-data)
      
      (or (key-down? :left-shift)
        (key-down? :right-shift))
      (select-char-after text-data)
      
      (move-char-after text-data)))  
  
  (when (key-pressed? :enter)
    (def code (string
                (text-data :text)
                (text-data :selected)
                (string/reverse (text-data :after))))
    (print "Eval! " code)
    (-> (try (do (fiber/setenv (fiber/current) (data :top-env))
                 (put data :latest-res (string (eval-string code))))
             ([err fib]
              (put data :latest-res (string "Error: " err))))
      print)))

(defn handle-mouse
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
