(use jaylib)
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")
(import ./text_api :prefix "")
(import ./input :prefix "")
(import ./highlight :prefix "")

(varfn render-rows
  [{:conf text-conf
    :position pos
    :sizes sizes
    :styles styles
    :rows rows
    :full-text text
    :default-color color}]
  (def [x y] pos)
  (def {:spacing spacing} text-conf)
  (var render-x 0)
  (var char @"a")
  (var active-styles @[])
  
  (loop [i :range [0 (length rows)]
         :let [{:words words :start start} (rows i)
               s (string/join words "")]]
    (loop [ci :range [0 (length s)]
           :let [c (s ci)
                 abs-i (+ start ci)
                 [w h] (sizes abs-i)
                 {:color style-color} (get styles abs-i (comptime {}))]]
      (put char 0 c)
      (when (not= char "\n")
        (draw-text text-conf char 
                   [(+ x render-x) (+ y (* 40 i))]
                   
                   (or style-color color)))
      (+= render-x w))
    (set render-x 0)))

(var hm nil)

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

(varfn get-mouse-pos
  [[x y] text-data text sizes ps rows x-offset y-offset]
  (let [row-i (binary-search-closest rows |(compare y (+ ($ :y) ($ :h) y-offset)))   
        {:start start :stop stop} (rows (min row-i (dec (length rows))))      
        column-i (binary-search-closest (array/slice ps start stop)
                                        |(compare x (+ ($ :center-x) x-offset)))]      
    (+ start column-i)))


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

(varfn render-textarea
  [conf props]
  (def {:y y :selected selected :text text :after after :conf text-conf} props)
  
  (def {:spacing spacing
        :size font-size} text-conf)
  
  (def {:colors colors} conf)
  
  (re-measure props)
  
  (stylize conf props)
  
  (textarea-handle-mouse md props)
  
  (def {:rows rows 
        :positions ps 
        :sizes sizes
        :current-row current-row} props)
  
  (let [x 10
        w 500
        h (+ (* (length rows) 40) 16)
        roundness 0.05
        segments 9
        diff 2]
    (draw-rectangle-rounded [x y w h] roundness segments (colors :border))
    (draw-rectangle-rounded [(+ x diff)
                             (+ y diff)
                             (- w (* 2 diff))
                             (- h (* 2 diff))]
                            roundness
                            segments (colors :background)))  
  
  (def selection-start (length text))
  (def selection-end (+ (length text) (length selected)))
  
  (each {:x rx :y ry :w w :h h} (range->rects ps sizes selection-start selection-end)
    (draw-rectangle-rec [(+ rx 30)
                         (+ ry y)
                         w h]
                        (colors :selected-text-background)))
  
  (render-rows props)
  
  ##(pp rows)
  
  (+= (props :blink) 1.1)
  
  (when (and (< (props :blink) 30)
             (empty? selected))
    (let [[wwx wwy] (get-caret-pos props)
          h ((rows current-row) :h)
          h (if (= 0 h) (get-in props [:conf :size]) h)]
      (draw-line-ex
       [(+ 30 wwx)
        (+ y wwy (* 0.15 font-size))]
       [(+ 30 wwx)
        (+ (+ y wwy (* 0.15 font-size))
           (* h 0.75))] 1 (colors :caret))))
  
  (when (> (props :blink) 60) (set (props :blink) 0)))
