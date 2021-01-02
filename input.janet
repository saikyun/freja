(use jaylib)
(import ./text_api :prefix "")
(import ./text_rendering :prefix "")
(import ./highlight :prefix "")
(import ./find_row_etc :prefix "")

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

(varfn eval-it
  [data code]
  (print "Eval! " code)
  (try (do (fiber/setenv (fiber/current) (data :top-env))
           (put data :latest-res (string (eval-string code))))
       ([err fib]
        (put data :latest-res (string "Error: " err))))
  (pp (data :latest-res)))

(varfn meta-down?
  []
  (if (= :macos (os/which))
    (or (key-down? :left-super)
        (key-down? :right-super))
    (or (key-down? :left-control)
        (key-down? :right-control))))

(comment
 
 (length (content text-data))
 
 (do (put binds :up (vertical-move |(max 0 (dec (row-of-pos ($ :rows)
                                                            (cursor-pos $))))
                                   (fn [_] 0)))
     
     (put binds :down (vertical-move
                       |(min (dec (length ($ :rows)))
                             (tracev (inc (row-of-pos ($ :rows)
                                                      (cursor-pos $)))))
                       |(length (content $)))))
 )

## stores held keys and the delay until they should trigger
(var delay-left @{})

## delay before first repetition of held keys
(var initial-delay 0.2)

## delay of each repetition thereafter
(var repeat-delay 0.03)

## bindings from key to function
(def binds @{:end (fn [props]
                    (if (or (key-down? :left-shift)
                            (key-down? :right-shift))
                      (select-until-end-of-line props)
                      (move-to-end-of-line props)))
             
             :left (fn [props]
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
             
             :right (fn [props]
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
             
             :v (fn [props]
                  (when (meta-down?)
                    (reset-blink props)
                    
                    (paste props)))
             
             :delete (fn [props]
                       (reset-blink props)
                       
                       (cond (or (key-down? :left-alt)
                                 (key-down? :right-alt))
                             (delete-word-after props)
                             
                             (forward-delete props))) 
             
             :a (fn [props]
                  (when (meta-down?)
                    (select-all props)))  
             
             :c (fn [props]
                  (when (meta-down?)
                    (copy props)))    
             
             :x (fn [props]
                  (when (meta-down?)
                    (reset-blink props)
                    
                    (cut props)))
             
             :e (fn [props]
                  (when (meta-down?)
                    (eval-it (props :data) (last (peg/match sexp-grammar (props :text))))))   
             
             :backspace (fn [props] (reset-blink props)
                          
                          (cond (or (key-down? :left-alt)
                                    (key-down? :right-alt))
                                (delete-word-before props)
                                
                                (backspace props)))
             
             :q (fn [props]
                  (when (or (key-down? :left-control)
                            (key-down? :right-control))
                    (put (props :data) :quit true)))

             
             :home (fn [props]
                     (reset-blink props)
                     
                     (if (or (key-down? :left-shift)
                             (key-down? :right-shift))
                       (select-until-beginning-of-line props)
                       (move-to-beginning-of-line props)))
             
             :enter (fn [props]
                      (reset-blink props)
                      
                      (cond
                        (or (key-down? :left-control)
                            (key-down? :right-control))    
                        (do (def code (string
                                       (props :text)
                                       (props :selected)
                                       (string/reverse (props :after))))
                            (eval-it (props :data) code))
                        
                        (insert-char props (first "\n"))))
             
             :up (vertical-move |(max 0 (dec (row-of-pos ($ :rows)
                                                         (cursor-pos $))))
                                (fn [_] 0))
             
             :down (vertical-move
                    |(min (dec (length ($ :rows)))
                          (inc (row-of-pos ($ :rows)
                                           (cursor-pos $))))
                    |(length (content $)))})

(varfn handle-keyboard
  [data dt]
  (def {:text-data props} data)
  
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
  
  (loop [k :keys binds]
    (when (key-released? k)
      (put delay-left k nil)
      (scroll-to-focus props))
    
    (when (key-pressed? k)
      (reset-blink props)    
      (put delay-left k initial-delay)
      ((binds k) props)
      
      (scroll-to-focus props))))


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
    (reset-blink text-data)
    
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

(varfn get-mouse-pos
  [[x y] text-data text sizes ps rows x-offset y-offset]
  (let [row-i (binary-search-closest rows |(compare y (+ ($ :y) ($ :h) y-offset)))   
        {:start start :stop stop} (rows (min row-i (dec (length rows))))      
        column-i (binary-search-closest (array/slice ps start stop)
                                        |(compare x (+ ($ :center-x) x-offset)))]      
    (+ start column-i)))

(varfn get-mouse-row
  [[_ y] rows y-offset]
  (binary-search-closest rows |(compare y (+ ($ :y) ($ :h) y-offset))))

(varfn handle-mouse
  [mouse-data props]
  (def {:full-text text
        :sizes sizes
        :positions ps
        :rows rows
        :position offset} props)
  (def [x-offset y-offset] offset)
  (def pos (get-mouse-position))
  (def [x y] pos)
  
  (let [move (get-mouse-wheel-move)]
    (update props :scroll |(max (- (or (-?> (props :rows) last (get :y)) 0))
                                (min 0 (+ $ (* move 10))))))
  
  (def y-offset (+ y-offset (props :scroll)))
  
  (comment
   (when (mouse-button-down? 0)
     (def row-i (binary-search-closest rows |(compare y (+ ($ :y) ($ :h) y-offset))))   
     (def {:start start :stop stop} (rows (min row-i (dec (length rows)))))      
     (def column-i (binary-search-closest (array/slice ps start stop)
                                          |(compare x (+ ($ :center-x) x-offset))))      
     
     (def pos (+ start column-i))      
     
     (move-to-pos props pos))
   ) 
  
  ## (def [x y] (get-mouse-position))
  
  (put mouse-data :just-double-clicked false)  
  (put mouse-data :just-triple-clicked false)  
  
  (def both (content props))    
  
  (when (mouse-button-released? 0)
    (put mouse-data :last-text-pos nil)
    (put mouse-data :just-down nil)
    (put mouse-data :recently-double-clicked nil)
    (put mouse-data :recently-triple-clicked nil)
    (put mouse-data :up-pos [x y])
    
    (when (mouse-data :down-pos)
      (put mouse-data :selected-pos [(get-mouse-pos
                                      (mouse-data :down-pos)
                                      props                                     
                                      text                                     
                                      sizes                                     
                                      ps                                     
                                      rows
                                      x-offset
                                      y-offset)
                                     (get-mouse-pos
                                      pos
                                      props                                     
                                      text                                     
                                      sizes                                     
                                      ps                                     
                                      rows                                     
                                      x-offset                                     
                                      y-offset)])))  
  
  (when (mouse-button-pressed? 0)
    (reset-blink props)
    
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
        (select-all props)
        
        (and (mouse-data :just-double-clicked)
             (not (key-down? :left-shift))
             (not (key-down? :right-shift)))
        (select-surrounding-word props)
        
        (or (mouse-data :recently-double-clicked)
            (mouse-data :recently-triple-clicked)) nil # don't start selecting until mouse is released again
        
        (mouse-button-down? 0)
        (do (when (nil? (mouse-data :last-text-pos))
              (put mouse-data :last-text-pos (length (props :text))))
            
            (put mouse-data :down-time (get-time))
            (if (= nil (mouse-data :just-down))
              (do (put mouse-data :just-down true)
                  (put mouse-data :down-pos [x y]))
              (put mouse-data :just-down false))
            
            (put mouse-data :selected-pos [(get-mouse-pos
                                            (mouse-data :down-pos)
                                            props                                     
                                            text                                     
                                            sizes                                     
                                            ps                                     
                                            rows                                     
                                            x-offset                                     
                                            y-offset)
                                           (get-mouse-pos
                                            pos
                                            props                                     
                                            text                                     
                                            sizes                                     
                                            ps                                     
                                            rows                                     
                                            x-offset                                     
                                            y-offset)])
            
            (var moved-caret false)
            
            (let [[start end] (mouse-data :selected-pos)
                  start (if (or (key-down? :left-shift)
                                (key-down? :right-shift))
                          (mouse-data :last-text-pos)
                          start)
                  
                  single (= start end)
                  p       (dec (get-in mouse-data [:selected-pos 1])) ## position before mouse
                  rp      (row-of-pos (props :rows) p)
                  newline (= (first "\n") (get both p))]
              
              (if (and single
                       newline
                       (= rp (get-mouse-row pos rows y-offset)))
                (select-region props (dec start) (dec end))
                (do
                  (if (= (row-of-pos (props :rows) (get-in mouse-data [:selected-pos 1]))
                         (get-mouse-row pos rows y-offset))
                    (put props :stickiness :down)
                    (put props :stickiness :right))
                  
                  (select-region props start end)))))))
