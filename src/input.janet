(use jaylib)
(import ./new_gap_buffer :prefix "")
(import ./render_new_gap_buffer :prefix "")
(import ./file_handling :prefix "")
(import ./code_api :prefix "")
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
         (def res (eval-string code))
         (put data :latest-res (string/format "%.4m" res)))
    ([err fib]
      (put data :latest-res err)))
  (pp (data :latest-res)))

(varfn meta-down?
  []
  (if (= :macos (os/which))
    (or (key-down? :left-super)
        (key-down? :right-super))
    (or (key-down? :left-control)
        (key-down? :right-control))))

## stores held keys and the delay until they should trigger
(var delay-left @{})

## delay before first repetition of held keys
(var initial-delay 0.2)

## delay of each repetition thereafter
(var repeat-delay 0.03)

(def kw->f {:paste (fn [props]
                     (when (meta-down?)
                       (reset-blink props)
                       
                       #(paste props)
                       ))})

## bindings from key to function
(def gb-binds @{:end (comment (fn [props]
                                (if (or (key-down? :left-shift)
                                        (key-down? :right-shift))
                                  (select-until-end-of-line props)
                                  (move-to-end-of-line props))))
                
                :s (fn [props]
                     (when (meta-down?)
                       ((props :open-file) props)))
                
                :left (fn [props]
                        (reset-blink props)
                        
                        (cond
                          ## select whole words
                          (and (or (key-down? :left-alt)
                                   (key-down? :right-alt))
                               (or (key-down? :left-shift)
                                   (key-down? :right-shift)))
                          (comment (select-word-before props))
                          
                          (or (key-down? :left-alt)
                              (key-down? :right-alt)) 
                          (backward-word! props)
                          
                          (or (key-down? :left-shift)
                              (key-down? :right-shift))
                          (select-backward-char! props)
                          
                          (backward-char! props)))
                
                :right (fn [props]
                         (reset-blink props)
                         
                         (cond 
                           (and (or (key-down? :left-alt)
                                    (key-down? :right-alt))
                                (or (key-down? :left-shift)
                                    (key-down? :right-shift)))
                           (comment (select-word-after props))
                           
                           (or (key-down? :left-alt)
                               (key-down? :right-alt))
                           (forward-word! props)
                           
                           (or (key-down? :left-shift)
                               (key-down? :right-shift))
                           (select-forward-char! props)
                           
                           (forward-char! props)))
                
                :delete (fn [props]
                          (reset-blink props)
                          
                          (cond (or (key-down? :left-alt)
                                    (key-down? :right-alt))
                            (comment (delete-word-after props))
                            
                            (comment (forward-delete props)))) 
                
                :a (fn [props]
                     (when (meta-down?)
                       #(select-all props)
                       ))  
                
                :b (fn [props]
                     (when (meta-down?)
                       (reset-blink props)
                       
                       #(cut props)
                       ))

                :i (fn [props]
                     (when (meta-down?)
                       #(copy props)
                       ))
                
                :. (kw->f :paste)
                
                :f (fn [props]
                     (when (meta-down?)
                       (reset-blink props)
                       
                       (print "formatting!")
                       
                       (format-code props)))
                
                :e (fn [props]
                     (when (meta-down?)
                       (eval-it (props :data) (last (peg/match sexp-grammar (props :text))))))   
                
                :backspace (fn [props] (reset-blink props)
                             
                             (cond (or (key-down? :left-alt)
                                       (key-down? :right-alt))
                               (comment (delete-word-before props))
                               
                               (backspace props)))
                
                :q (fn [props]
                     (when (or (key-down? :left-control)
                               (key-down? :right-control))
                       (put (props :data) :quit true)))
                
                
                :home (fn [props]
                        (reset-blink props)
                        
                        # (if (or (key-down? :left-shift)
                        #         (key-down? :right-shift))
                        #   (select-until-beginning-of-line props)
                        #   (move-to-beginning-of-line props))
                        
                        )
                
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
                
                :up   (fn [props]
                        (reset-blink props)
                        (move-up! props))
                :down (fn [props]
                        (reset-blink props)
                        (move-down! props))
                
                #:up (vertical-move previous-row (fn [_] 0))
                
                # :down (vertical-move
                #         next-row
                #         |(length (content $)))
                
                
                :page-up (fn [props]
                           #(page-up props)
                           )             
                
                :page-down (fn [props]
                             #(page-down props)
                             )})

(comment
  (put gb-data :binds gb-binds)
  )

(defn add-bind
  ``
  Take a button kw, and a function or a key in `kw->f`. Adds that key and function to the global bindings.
  
  Examples:
  `(add-bind :b :paste)`
  `(add-bind :c (fn [props] (pp props)))`
  `(add-bind :c (fn [props] (when (meta-down?) (print "meta-c!"))))`
  ``
  [binds button f-or-kw]
  (def f (if (keyword? f-or-kw) (kw->f f-or-kw) f-or-kw))
  (put binds button f))

(def game-binds @{})
(def pressed-game-binds @{})

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


(varfn handle-scroll
  [props]
  (let [move (get-mouse-wheel-move)]
    (when (not= move 0)
      (update props :scroll |(min 0 (+ $ (* move 10))))
      (put props :changed true))))

(varfn handle-mouse
  [mouse-data props]
  (def {:lines lines
        :offset offset
        :position position
        :y-poses y-poses
        :sizes sizes
        :scroll scroll} props)
  
  (def [ox oy] offset)
  (def [x-pos y-pos] position)
  (def pos (get-mouse-position))
  (def [x y] pos)
  
  (def y-offset (+ y-pos oy scroll))
  (def x-offset (+ x-pos ox))
  
  (put mouse-data :just-double-clicked false)  
  (put mouse-data :just-triple-clicked false)  
  
  (when (mouse-button-released? 0)
    (put mouse-data :last-text-pos nil)
    (put mouse-data :just-down nil)
    (put mouse-data :recently-double-clicked nil)
    (put mouse-data :recently-triple-clicked nil)
    (put mouse-data :up-pos [x y])
    
    (comment
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
                                         y-offset)]))))  
  
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
    (comment (select-all props))
    
    (and (mouse-data :just-double-clicked)
         (not (key-down? :left-shift))
         (not (key-down? :right-shift)))
    (comment (select-surrounding-word props))
    
    (or (mouse-data :recently-double-clicked)
        (mouse-data :recently-triple-clicked)) nil # don't start selecting until mouse is released again
    
    (mouse-button-down? 0)
    (do 
      (when (nil? (mouse-data :last-text-pos))
        (put mouse-data :last-text-pos (+ (props :gap-start)
                                          (length (props :gap)))))
      
      (put mouse-data :down-time (get-time))
      
      (if (= nil (mouse-data :just-down))
        (do (put mouse-data :just-down true)
          (put mouse-data :down-pos [x y]))
        (put mouse-data :just-down false))
      
      (print "y-offset: " y-offset)
      
      (let [line-index (max 0 (dec (binary-search-closest y-poses |(compare y (+ $ y-offset)))))
            row-start-pos (if (= 0 line-index)
                            0
                            (lines (dec line-index)))
            row-end-pos (lines line-index)]
        
        (put-gap-pos! props (index-passing-middle-max-width props
                                                            row-start-pos
                                                            row-end-pos
                                                            (- x ox))))
      
      (comment (put mouse-data :selected-pos [(get-mouse-pos
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
                   (= rp (y->row props (pos 1))))
            (select-region props (dec start) (dec end))
            (do
              (if (= (row-of-pos (props :rows) (get-in mouse-data [:selected-pos 1]))
                     (y->row props (pos 1)))
                (put props :stickiness :down)
                (put props :stickiness :right))
              
              (select-region props start end)))))))
  
  
  
  )
