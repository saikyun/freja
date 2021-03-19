(varfn handle-keyboard
  [data props dt]
  
  (def {:binds binds} props)
  
  (put props :data data)
  
  (var k (get-key-pressed))
  
  (while (not= 0 k)
    (reset-blink props)
    
    (cond (or (key-down? :left-shift)
              (key-down? :right-shift))
      (insert-char-upper props k)
      
      (insert-char props k))
    
    (scroll-to-focus props)
    
    (set k (get-key-pressed)))
  
  (loop [[k dl] :pairs delay-left
         :let [left ((update delay-left k - dt) k)]]
    (when (<= left 0)
      ((binds k) props)
      (put delay-left k repeat-delay)
      (scroll-to-focus props)))
  
  (when (key-released? :;)
    (print "YEE"))
  
  (loop [k :keys binds]
    (when (key-released? k)
      (put delay-left k nil)
      (scroll-to-focus props))
    
    (when (key-pressed? k)
      (reset-blink props)    
      (put delay-left k initial-delay)
      ((binds k) props)
      
      (scroll-to-focus props)))
  
  (loop [k :keys game-binds]
    (when (key-down? k)
      (print "wat?")
      ((game-binds k))))
  
  (loop [k :keys pressed-game-binds]
    (when (key-pressed? k)
      (print "wat 2?")
      ((pressed-game-binds k)))))
