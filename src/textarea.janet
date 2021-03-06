(use jaylib)
(import spork/test)
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")
(import ./text_api :prefix "")
(import ./input :prefix "")
(import ./highlight :prefix "")

(varfn render-rows
  [props]
  (def {:conf text-conf
        :debug debug
        :position pos
        :offset offset
        :sizes sizes
        :styles styles
        :rows rows
        :full-text text
        :default-color color
        :scroll scroll
        :no-render-since-change no-render-since-change}
    props)
  (def [x y] pos)
  (def [ox oy] offset)
  (def {:spacing spacing} text-conf)
  (var render-x 0)
  (var char @"a")
  (var active-styles @[])
  
  (def sh (get-screen-height))
  
  
  (when debug
    (when no-render-since-change
      (print "rendering rows"))
    )
  
  (loop [i :range [0 (length rows)]
         :let [{:words words :start start :y row-y} (rows i)
               s (string/join words "")]]
    (loop [ci :range [0 (length s)]
           :let [c (s ci)
                 abs-i (+ start ci)
                 [w h] (sizes abs-i)
                 {:color style-color} (get styles abs-i (comptime {}))
                 render-y (+ y oy scroll row-y)]
           :when (pos? (+ render-y h))]
      (when (> render-y sh)
        (break))
      (put char 0 c)
      (when (not= char "\n")
        (draw-text text-conf char 
                   [(+ ox x render-x) render-y]
                   
                   (or style-color color)))
      (+= render-x w))
    (set render-x 0))

  (put props :no-render-since-change false)

  )

(var hm nil)

(comment
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
          (select-region text-data start end))))))

### external
## color configuration
## mouse data

### internal
## caret position
## styling
## text data
## offset of "component"
## logical rows          3
## sizes                 3
## positions             3

(varfn textarea-handle-mouse
  [mouse-data props]
  (def {:full-text text
        :sizes sizes
        :positions ps
        :rows rows
        :position offset} props)
  (def [x-offset y-offset] offset)
  (def pos (get-mouse-position))
  (def [x y] pos)
  
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
                                     y-offset)]))  
  
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
                    start)]
        (select-region props start end)))))

(var md (new-mouse-data))

(varfn stylize
  [conf props]
  (def {:full-text full-text 
        :text text 
        :selected selected 
        :rows rows} props)
  (def {:colors colors} conf)
  (def styles @{})      
  
  (try
    (let [matches (peg/match styling-grammar full-text)]
      (each {:start start :stop stop :kind kind} matches
        (loop [i :range [start stop]]
          (put styles i {:kind kind :color (colors kind)}))))
    ([err fib]
      (pp err)
      (print "peg/match err")))    
  
  (loop [i :range [(length text) (+ (length text) (length selected))]]
    (put styles i {:color (colors :selected-text)}))
  
  (put props :styles styles)
  (put props :default-color (colors :text)))

(varfn focused?
  [{:context context :id id}]
  (= (context :focus) id))

(varfn render-textarea
  [conf props]
  (def {:position pos
        :changed changed
        :context context
        :offset offset
        :w w
        :h h
        :debug debug
        :selected selected
        :text text
        :after after
        :conf text-conf
        :scroll scroll} props)
  (def [x y] pos)
  (def [ox oy] offset)
  
  (def {:spacing spacing
        :size font-size} text-conf)
  
  (def {:colors colors} conf)
  
  (def maybe-time (if debug test/timeit identity))
  
  (when debug
    (print "re-measure"))
  (maybe-time
    (re-measure props))
  
  (when debug
    (print "stylize"))
  (maybe-time
    (stylize conf props))
  
  #(textarea-handle-mouse md props)
  
  (def {:rows rows 
        :positions ps 
        :sizes sizes
        :current-row current-row} props)
  (when debug
    (print "beginning"))
  (maybe-time
    (let [h (or (-?> h (- 5 y))
                (- (get-screen-height) y 5))
          roundness 0.015
          segments 9
          diff 2]
      
      (rlgl-draw)
      (rl-enable-scissor-test)
      (let [[x-scale _ _ _ _ y-scale] (get-screen-scale)] # returns a matrix with a bunch of zeroes
        (rl-scissor (* x-scale x)
                    (* y-scale (- (get-screen-height) (+ y h)))
                    (* x-scale w) (* y-scale h)))
      
      
      #(begin-scissor-mode 0 -690 (* w 100) (* h 100))
      #(begin-scissor-mode 0 0 (* w 100) (* h 100))
      
      (put props :calculated-h h)
      
      (draw-rectangle-rounded [x y w h] roundness segments (colors :border))
      (draw-rectangle-rounded [(+ x diff)
                               (+ y diff)
                               (- w (* 2 diff))
                               (- h (* 2 diff))]
                              roundness
                              segments (colors :textarea))))
  
  (when debug
    (print "selection"))
  (def selection-start (length text))
  (def selection-end (+ (length text) (length selected)))
  
  (maybe-time
    (each {:x rx :y ry :w w :h h} (range->rects props selection-start selection-end)
      (let [w (if (= w 0) 5 w)]
        (draw-rectangle-rec [(+ rx x ox)
                             (+ ry y 
                                (- (* h 1 (dec (text-conf :line-height)))) # compensate for line height
                                oy scroll)
                             w
                             (* h (+ 1 (* 1 (dec (text-conf :line-height)))))]
                            (colors :selected-text-background)))))
  
  (when debug
    (print "render-rows"))
  (maybe-time
    (render-rows props))
  
  ##(pp rows)
  
  (when debug
    (print "caret"))
  (+= (props :blink) 1.1)
  
  (maybe-time
    (when (and (< (props :blink) 30)
               (empty? selected)
               (focused? props))
      (let [[wwx wwy] (get-caret-pos props)
            h (get-in rows [current-row :h] 0)
            h (if (= 0 h) (* (get-in props [:conf :size]) 0.5) h)]
        (draw-line-ex
          [(+ ox x wwx)
           (+ oy
              y
              scroll
              wwy
              (- (* h 1 (dec (text-conf :line-height)))))]
          [(+ ox x wwx)
           (+ (+ oy y scroll wwy)
              (* h (+ 1 (* 0.5 (dec (text-conf :line-height))))))] 1 (colors :caret)))))
  
  (when (> (props :blink) 60) (set (props :blink) 0))
  
  (end-scissor-mode)
  )
