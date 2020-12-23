(use ./build/jaylib)
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")
(import ./text_api :prefix "")
(import ./input :prefix "")
(import ./highlight :prefix "")

(varfn split-words
  [t]
  (peg/match '(any (+ (capture (some (if-not :s 1)))
                     (capture (* (if-not "\n" :s)))
                     (capture "\n"))) t))

(varfn measure-each-char
  [conf whole-text]
  (def {:size size :spacing spacing} conf)
  (var arr (buffer/new 1))
  (seq [i :range [0 (length whole-text)]
        :let [c (whole-text i)]]
    (put arr 0 c)
    (if (= c (first "\n"))
      (let [[x y] (measure-text conf " ")]
        [0 y])
      (let [[x y] (measure-text conf arr)]
        [(+ x 2) y]))))

(varfn size-between
  [sizes start stop]
  (var size @{:w 0 :h 0})
  (loop [i :range [start stop]
         :let [[w h] (sizes i)]]
    (-> (update size :w + w)
      (update :h max h)))
  size)

(varfn index-before-max-width
  [sizes start stop max-width]
  (var ret 0)
  (var acc-w 0)
  (loop [i :range [start stop]
         :let [[w h] (sizes i)]]
    (+= acc-w w)
    (when (> acc-w max-width)
      (set ret (dec i))
      (break)))
  ret)

(varfn wordwrap
  [sizes words max-width]
  (var rows @[@{:y 0 :h 0 :words @[]
                :start 0 :end 0}])
  (var start 0)
  (var curr-w 0)
  (var acc-y 0)
  (var max-h 0)
  
  (defn add-word [word stop {:w w :h h}]
    (update (last rows) :h max h)
    (+= curr-w w)
    
    (if (= word "\n")
      (do (set curr-w w)
          (array/push ((last rows) :words) word)
          (def new-y (+ acc-y ((last rows) :h)))
          (put (last rows) :stop stop)
          (set start stop)
          (array/push rows @{:y new-y :h h :words @[]
                             :start start
                             :stop stop})
          (set acc-y new-y))
      (do (when (> curr-w max-width)
            (put (last rows) :word-wrapped true)
            (def new-y (+ acc-y ((last rows) :h)))
            (if (> w max-width)
              (let [i (index-before-max-width sizes start stop max-width)
                    p (- i start)]
                (loop [word :in [(string/slice word 0 p) (string/slice word p)]
                       :let [stop (+ start (length word))
                             size (size-between sizes start stop)]]
                  (add-word word stop size)))
              (do (when (not (empty? ((last rows) :words)))
                    (array/push rows @{:y new-y :h 0 :words @[]
                                       :start start
                                       :stop stop})
                    (set acc-y new-y))
                  (set curr-w w))))
          
          (when (not (> w max-width))
            (array/push ((last rows) :words) word))))    
    
    (set start stop))
  
  (loop [word :in words
         :let [stop (+ start (length word))
               size (size-between sizes start stop)]]
    (add-word word stop size)
    (put (last rows) :stop stop))
  
  rows)

(varfn char-positions
  [sizes rows]
  (var i 0)
  (var ps @[])
  (loop [{:y y-pos :words words} :in rows
         :let [nof (+ ;(map length words))
               stop (+ i nof)]]
    (var acc-x 0)
    (for i2 i stop
      (def [x y] (sizes i2))
      (put ps i2 {:x acc-x :y y-pos
                  :center-x (+ acc-x (* x 0.5))})
      (set acc-x (+ acc-x x)))
    (+= i nof))
  ps)

(varfn range->rects
  [ps sizes start stop]
  (var rects @[])
  (loop [i :range [start stop]
         :let [{:x x :y y} (ps i)
               [w h] (sizes i)
               r (last rects)]]
    (if (= (get r :y) y)
      (-> r
        (put :w (- (+ x w) (r :x)))
        (update :h max h))
      (array/push rects @{:x x :y y :w w :h h})))
  rects)

## need a function that gets me all
## positions of all chars
## ie like measure-each-char but with positions instead

(comment
  
  (calc-splits (conf :text) t nil)
  
  (do
    (def t "hnsaoe             aoehtsn")
    (def sizes (measure-each-char (conf :text) t))
    (size-between sizes 0 5)
    (def words (split-words t))
    (def rows (wordwrap sizes words 100))
    (def ps (char-positions sizes rows))
    
    (range->rects ps sizes 0 10))
  
  )

(comment
  
  (break-up-words (conf :text) @"" 0 100 1000)
  
  (split-words @"")
  
  (reduce (fn [acc c]
            
            (print c)) 0 text)
  
  (break-up-words "a . b" 40 2 100)
  (break-up-words text 40 2 100)
  
  (-> (poses-in-text "aoeu aoeu" 40 2)
    (break-up 100))
  
  (-> (poses-in-text "aoeu eoatnheoasnhaoeu" 40 2)
    (break-up 100))
  
  (-> (poses-in-text text 40 2)
    (break-up 2 100)
    length
    )
  
  )

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

(varfn textarea-handle-keyboard
  [props]
  (def {:caret-pos caret-pos
        :full-text text
        :sizes sizes
        :positions ps
        :current-row current-row
        :rows rows
        :position offset}
    props)
  (def [x y] caret-pos)
  (def [x-offset y-offset] offset)
  
  (when (key-pressed? :right)
    
    (put props :caret-pos (get-caret-pos props))
    (pp (props :caret-pos))
    )
  
  (when (key-pressed? :left)
    
    (put props :caret-pos (get-caret-pos props))
    (pp (props :caret-pos))
    )
  
  (when (key-pressed? :up)
    (print "current row " current-row)    
    (pp caret-pos)
    
    (reset-blink props)
    
    (def new-row (max 0 (dec current-row)))
    (def {:start start :stop stop} (rows new-row))
    (def column-i (binary-search-closest (array/slice ps start stop)
                    |(compare x ($ :center-x))))
    (var pos (+ start column-i))
    
    (when (or (= (first "\n") (get text (dec pos)))
            (and (get-in rows [new-row :word-wrapped])
              (= pos (get-in rows [new-row :stop]))))
      (print "caretting up " (caret-pos 0))
      (when (<= 0 (caret-pos 0))
        (pp caret-pos)
        (-= pos 1)))
    
    (put-in props [caret-pos 1] new-row)
    (move-to-pos props pos)
    
    (print "new row " new-row))
  
  (when (key-pressed? :down)
    (print "current row " current-row)
    (pp caret-pos)
    
    (reset-blink props)
    
    (def new-row (min (dec (length rows)) (inc current-row)))
    (def {:start start :stop stop} (rows new-row))      
    (def column-i (binary-search-closest (array/slice ps start stop)
                    |(compare x ($ :center-x))))
    
    (var pos (+ start column-i))
    
    (when (or (= (first "\n") (get text (dec pos)))
            (and (get-in rows [new-row :word-wrapped])
              (= pos (get-in rows [new-row :stop]))))
      (print "carretting")
      (when (<= 0 (caret-pos 0))
        (pp caret-pos)
        (-= pos 1)))
    
    (put-in props [caret-pos 1] new-row)
    (move-to-pos props pos)
    
    (print "new row " new-row)))

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

(varfn render-textarea
  [conf props]
  (def {:y y :selected selected :text text :after after :conf text-conf} props)
  
  (def {:spacing spacing
        :size font-size} text-conf)
  
  (def {:colors colors} conf)
  
  (def all-text (let [v (buffer text selected (string/reverse after))]
                  (if (empty? v)   ## `(peg/match ... (buffer @""))` breaks for some reason
                    @""
                    v)))
  
#(def rows (break-up-words text-conf all-text 0 280 (dec (length text))))
  
  (def sizes (measure-each-char (conf :text) all-text))
#(size-between sizes 0 5)
  (def words (split-words all-text))
  (def rows (wordwrap sizes words 450))
  
  (def ps (char-positions sizes rows))
  
  (def styles @{})  
  
  (try
    (let [matches (peg/match styling-grammar all-text)]
      (each {:start start :stop stop :kind kind} matches
            (loop [i :range [start stop]]
              (put styles i {:kind kind :color (colors kind)}))))
    ([fib err]
     (print "peg/match err")))
  
  (loop [i :range [(length text) (+ (length text) (length selected))]]
    (put styles i {:color (colors :selected-text)}))
  
  
  (var current-row 0)
  (loop [i :range [0 (length rows)]
         :let [r (rows i)]]
    (when (and (>= (max (dec (length text)) 0) (r :start))
            (< (max (dec (length text)) 0) (r :stop)))
      (set current-row i)
      (break))
    
    ## it's the last, empty row
    (set current-row i))

  (when (= (first "\n") (last text))
    (+= current-row 1))
  
  
  
  (put props :current-row current-row)
  (put props :full-text all-text)     
  (put props :sizes sizes)
  (put props :positions ps)
  (put props :styles styles)
  (put props :rows rows)
  (put props :position [30 y])
  (put props :default-color (colors :text))
  
  (textarea-handle-keyboard props)
  
  (textarea-handle-mouse md props)
  
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
