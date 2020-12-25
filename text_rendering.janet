(use ./build/jaylib)
#(import ./text_api :prefix "")

(defn measure-text
  [tc text]
  (measure-text-ex (tc :font) text (tc :size) (tc :spacing)))

(defn draw-text
  [tc text pos color]
  (draw-text-ex (tc :font) text pos (tc :size) (tc :spacing) color))

(varfn get-pos-in-text
  [text-data x]
  (def {:conf conf :offset offset :text text :selected selected :after after} text-data)
  (def {:size font-size :spacing spacing} conf)
  
  (var total-w 0)
  (var last -10000)
  
  (def both (string text selected (string/reverse after)))
  
  (var res (length both)) # no matches = far right
  
  (if (<= x offset)
    0
    (do (loop [i :range [0 (length both)]
               :let [s2 (string/slice both i (inc i)) 
                     [w2 h] (measure-text conf s2)
                     w3 (math/ceil (/ w2 2))]]
          (set total-w (+ total-w w2))
          
          ## (draw-line-ex
          ##   [(- (+ offset total-w) w2) 60]
          ##   [(- (+ offset total-w) w2) (+ 60 (* h 0.75))]
          ##   1
          ##   (colors :caret))            
          
          ## (draw-line-ex
          ##   [(- (+ offset total-w) w3) 60]
          ##   [(- (+ offset total-w) w3) (+ 60 (* h 0.75))]
          ##   1
          ##   :blue)
          
          ## (draw-line-ex
          ##   [(+ offset total-w) 60]
          ##   [(+ offset total-w) (+ 60 (* h 0.75))]
          ##   1
          ##   (colors :caret))
          
          (when (and (<= x (- (+ total-w offset) w3))
                  (>= x last))
            (set res i)
            (break))
          
          (set last (- total-w w3))
          
          (if (not= i (dec (length both)))
            (set total-w (+ total-w spacing))))
        res)))

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

(varfn get-caret-pos
  [{:current-row current-row
    :rows rows
    :positions ps
    :sizes sizes
    :conf text-conf
    :text text}]
  (def {:size font-size
        :spacing spacing} text-conf)
  
  (let [newline (or (= (first "\n") (last text))
                  (and (get-in rows [(dec current-row) :word-wrapped])
                    (= (length text) (get-in rows [(dec current-row) :stop]))))]
    (when-let [{:x cx :y cy} (or (when-let [pos (get ps (max (dec (length text)) 0))]
                                   (if newline
                                     (let [h ((get rows current-row {:h 0}) :h)]
                                       {:x 0 :y (+ (pos :y) h)})
                                     pos))
                               {:x 0 :y 0})]
      (let [s (get sizes (dec (length text)))
            w (if newline 0 (get s 0 0))]
        [(- (+ cx w) (* spacing 0.5)) cy]))))

(varfn re-measure
  [props]
  (def {:text text :selected selected :after after :conf conf} props)
  (def all-text (let [v (buffer text selected (string/reverse after))]
                  (if (empty? v)   ## `(peg/match ... (buffer @""))` breaks for some reason
                    @""
                    v)))
  
#(def rows (break-up-words text-conf all-text 0 280 (dec (length text))))
  
  (def sizes (measure-each-char conf all-text))
#(size-between sizes 0 5)
  (def words (split-words all-text))  
  (def rows (wordwrap sizes words 450))  
  
  (def ps (char-positions sizes rows))  
  
  (var current-row 0)  
  (loop [i :range [0 (length rows)]
         :let [r (rows i)]]
    (when (and (>= (max (dec (length text)) 0) (r :start))
            (< (max (dec (length text)) 0) (r :stop)))
      (set current-row i)
      (break))
    
    ## it's the last, empty row
    (set current-row i))  
  
  (when (or (= (first "\n") (last text))
          (and (get-in rows [current-row :word-wrapped])
            (= (length text) (get-in rows [current-row :stop]))))
    (+= current-row 1))  
  
  (put props :current-row current-row)  
  (put props :full-text all-text)       
  (put props :sizes sizes)  
  (put props :positions ps)  
  (put props :rows rows)  
  (put props :position [30 (props :y)]))

(varfn refresh-caret-pos
  [props]
  (re-measure props)
  (put props :caret-pos (get-caret-pos props)))

