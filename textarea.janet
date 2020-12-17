(use ./build/jaylib)
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")
(import ./text_api :prefix "")

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
    (def [x y] (measure-text conf arr))
    [(+ x 2) y]))

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
    (when (> curr-w max-width)
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
      (array/push ((last rows) :words) word))    
    
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
  [text-conf {:x x :y y} rows text color]
  (def {:spacing spacing} text-conf)
  (var nof 0)
  (var new-x x)
  (var render-x x)
  (var new-y y)
  (when (not (or (empty? rows)
               (and (= (length rows) 1)
                 (empty? (first rows)))))
    (loop [i :range [0 (length rows)]
           :let [l (+ ;(map (comp length first) (get rows i)))
                 w (+ ;(map |(get $ 1) (get rows i))
                     (- (* spacing (length (get rows i))) spacing))]]
      (draw-text text-conf (string/slice text nof (+ nof l)) [(+ 30 render-x) (+ y (* 40 i))]
        color)
      (+= nof l)
      (when-let [v (get (last (get rows i)) 1)]
        (set new-x (+ render-x w)))
      (set render-x 0)
      (set new-y (+ y (* 40 i)))))
  {:x (if (pos? nof)
        (+ new-x spacing)
        new-x)
   :y new-y})

(varfn render-rows
  [text-conf {:x x :y y} rows text color]
  (def {:spacing spacing} text-conf)
  (var render-x x)
  (loop [i :range [0 (length rows)]
         :let [{:words words} (rows i)
               s (string/join words "")]]
    (draw-text text-conf s [(+ 30 render-x) (+ y (* 40 i))] color)
    (set render-x 0)))

(var hm nil)

(varfn textarea-handle-mouse
  [text-data text sizes ps rows x-offset y-offset]
  (def [x y] (get-mouse-position))
  
  (when (mouse-button-down? 0)
    (def row-i (binary-search-closest rows |(compare y (+ ($ :y) ($ :h) y-offset))))
    (def {:start start :stop stop} (rows (min row-i (dec (length rows)))))
    (def column-i (binary-search-closest (array/slice ps start stop)
                    |(compare x (+ ($ :center-x) x-offset))))
    
    (move-to-pos text-data (+ start column-i))))

(varfn render-textarea
  [conf text-data {:y y}]
  (def {:selected selected :text text :after after :conf text-conf} text-data)
  
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
  
  (textarea-handle-mouse text-data all-text sizes ps rows 30 y)
  
  (let [x 10
        w 500
        h (* (length rows) 40)
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
  
  (let [[w h] (measure-text text-conf text)
        [w2 h2] (measure-text text-conf selected)]
    (var x-ye 0)
    (var y-ye 0)
    
    (def selection-start (length text))
    (def selection-end (+ (length text) (length selected)))
    
    (each {:x rx :y ry :w w :h h} (range->rects ps sizes selection-start selection-end)
          (draw-rectangle-rounded [(+ rx 30)
                                   (+ ry y)
                                   w h]
            0.3
            9
            (colors :selected-text-background)))
    
    (render-rows text-conf {:x 0 :y y} rows all-text (colors :text))
    
# (def new-pos (render-rows text-conf {:x 0 :y y} rows all-text (colors :text)))
    (comment
      (draw-rectangle-rounded [(+ 30 (new-pos :x) spacing)
                               (+ (new-pos :y) (* 0.15 font-size))
                               (+ spacing w2)
                               (- h2 10)]
        0.3
        9
        (colors :selected-text-background)))    
    
#    (def rows-selected (break-up-words text-conf selected (new-pos :x) 280))
    
#    (def new-pos (render-rows text-conf new-pos rows-selected selected (colors :selected-text)))
    
#    (def rows-after (break-up-words text-conf (string/reverse after) (new-pos :x) 280))
    
#(draw-text text-conf selected [(+ 30 spacing w) y] (colors :selected-text))
    
#    (render-rows text-conf new-pos rows-after (string/reverse after)  (colors :text))
    
#(draw-text text-conf (string/reverse after) [(+ 30 spacing w spacing w2) y] (colors :text))
    
    (when-let [{:x cx :y cy} (get ps (max (dec (length text)) 0))]
      (let [s (get sizes (dec (length text)))
            w (get s 0 0)
            h (get s 1 font-size)]
        (draw-line-ex [(- (+ 30 cx w) (* spacing 0.5))
                       (+ cy y (* 0.15 font-size))
                       ] [(- (+ 30 cx w) (* spacing 0.5))
                          (+ #font-size
                            (+ cy y (* 0.15 font-size))
                            (* h 0.75))
                          
                          ] 1 (colors :caret))))
    
    (comment
      (draw-line-ex [(+ 30 spacing #w
                       (new-pos :x)
                       ) #font-size
                     (+ (new-pos :y) (* 0.15 font-size))
                     ] [(+ 30 spacing #w
                          (new-pos :x)
                          )
                        (+ #font-size
                          (+ (new-pos :y) (* 0.15 font-size))
                          (* h 0.75))
                        
                        ] 1 (colors :caret)))))
