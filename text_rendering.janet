(use jaylib)
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
          
          # (draw-line-ex
          #  [(- (+ offset total-w) w2) 60]
          #  [(- (+ offset total-w) w2) (+ 60 (* h 0.75))]
          #  1
          #  (colors :caret))            
          
          # (draw-line-ex
          #  [(- (+ offset total-w) w3) 60]
          #  [(- (+ offset total-w) w3) (+ 60 (* h 0.75))]
          #  1
          #  :blue)
          
          # (draw-line-ex
          #  [(+ offset total-w) 60]
          #  [(+ offset total-w) (+ 60 (* h 0.75))]
          #  1
          #  (colors :caret))
          
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
  (var ret start)
  (var acc-w 0)
  (loop [i :range [start stop]
         :let [[w h] (sizes i)]]
    (+= acc-w w)
    (when (> acc-w max-width)
      (set ret i)
      (break)))
  ret)

(varfn regular-word
  [state word {:w w}]
  (let [row (last (state :rows))]
    (-> row
        (put :stop (+ (row :stop) (length word)))
        (update :w + w)
        (update :words array/push word))))

(varfn handle-newline
  [state word {:w w :h h}]
  (let [row    (last (state :rows))
        start  (row :stop)
        stop   (+ start (length word))
        new-y  (+ (row :y) (row :h))]
    
    (array/push (row :words) word)
    (put row :stop stop)
    
    (update state :rows array/push @{:y new-y
                                     :h h
                                     :w 0
                                     :words @[]
                                     :start stop
                                     :stop  stop})))

(varfn add-word [w i] nil)

(varfn handle-wide-word
  [state word {:h h}]
  (let [row   (last (state :rows))
        start (row :stop)
        stop  (+ start (length word))
        i (index-before-max-width (state :sizes) start stop (state :max-width))
        p (- i start)]
    
    (loop [word :in [(string/slice word 0 p) (string/slice word p)]
           :let [row (last (state :rows))
                 new-y (+ (row :y) (row :h))
                 start (row :stop)]]
      
      (when (not (empty? (row :words)))
        (update state :rows array/push @{:y new-y
                                         :h h
                                         :w 0
                                         :word-wrapped false
                                         :words @[]
                                         :start start
                                         :stop  start}))
      
      (add-word state word))
    
    state))

(varfn handle-wide-line
  [state word size]
  (let [{:w w :h h} size
        {:rows rows 
         :max-width max-width} state
        row (last rows)
        new-y (+ (row :y) (row :h))
        start (row :stop)]
    
    (put row :word-wrapped true)
    
    (cond (> w max-width)
          (handle-wide-word state word size)
          
          (not (empty? (row :words)))
          (-> state
              (update :rows array/push @{:y new-y
                                         :h h
                                         :w 0
                                         :words @[]
                                         :start start
                                         :stop  start})
              (add-word word))
          
          (add-word state word))))

(varfn add-word [state word]
  (let [{:rows rows
         :max-width max-width} state
        
        row (last rows)
        
        start (row :stop)
        
        stop  (+ start (length word))
        size  (size-between (state :sizes) start stop)
        
        {:w w :h h} size
        curr-w (+ (row :w) w)]
    
    (update row :h max h)
    
    (cond (= word "\n")
          (handle-newline state word size)
          
          (> curr-w max-width)
          (handle-wide-line state word size)
          
          (regular-word state word size))))

(varfn wordwrap
  [sizes words max-width]
  (var rows @[@{:y 0
                :h 0
                :w 0
                :words @[]
                :start 0
                :stop 0}])
  
  (def state @{:max-width max-width
               :sizes sizes
               :rows rows})
  
  (loop [word :in words]
    (add-word state word))
  
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
    :text text
    :stickiness stickiness}]
  (def {:size font-size
        :spacing spacing} text-conf)
  
  (let [newline (= (first "\n") (last text))
        word-wrapped-down (and (= stickiness :down)
                               (get-in rows [(dec current-row) :word-wrapped])
                               (= (length text) (get-in rows [(dec current-row) :stop])))
        word-wrapped-right (and (= stickiness :right)
                                (get-in rows [current-row :word-wrapped])
                                (= (length text) (get-in rows [current-row :stop])))]
    (when-let [{:x cx :y cy} (or (when-let [pos (get ps (max (dec (length text)) 0))]
                                   (cond newline
                                         (let [h ((get rows current-row) :h)]
                                           {:x 0 :y (+ (pos :y) h)})
                                         
                                         word-wrapped-down
                                         (let [h ((get rows current-row) :h)]
                                           {:x 0 :y (+ (pos :y) h)})
                                         
                                         word-wrapped-right
                                         pos
                                         
                                         pos))
                                 {:x 0 :y 0})]
      (let [s (get sizes (dec (length text)))
            w (if (or newline word-wrapped-down) 0 (get s 0 0))]
        [(- (+ cx w) (* spacing 0.5)) cy]))))

(varfn row-of-pos
  [rows pos]
  (var current-row 0)
  (loop [i :range [0 (length rows)]
         :let [r (rows i)]]
    (when (and (>= (max pos 0) (r :start))
               (< (max pos 0) (r :stop)))
      (set current-row i)
      (break))
    
    ## it's the last, empty row
    (set current-row i))
  current-row)

(varfn cursor-pos
  "Returns the position of the cursor, which depends on the direction of selection."
  [props]
  (if (= :right (props :dir))
    (+ (length (props :text))
       (length (props :selected)))
    (length (props :text))))

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
  
  (def cp (cursor-pos props))
  
  (var current-row 0)
  (loop [i :range [0 (length rows)]
         :let [r (rows i)]]
    (when (and (>= (max (dec cp) 0) (r :start))
               (< (max (dec cp) 0) (r :stop)))
      (set current-row i)
      (break))
    
    ## it's the last, empty row
    (set current-row i))
  
  (put props :non-moved-row current-row)
  
  (when (and (not (empty? all-text))
             (= (first "\n")
                (all-text (max (dec cp) 0))
                #(last text)
                ))
    (+= current-row 1))
  
  (when (and (get-in rows [current-row :word-wrapped])
             (= cp (get-in rows [current-row :stop]))
             (= (props :stickiness) :down))
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

(varfn reset-blink
  [props]
  (set (props :blink) 0))

(varfn scroll-to-focus
  [props]
  (let [curr-y (get ((props :rows) (props :current-row)) :y 0)
        curr-h (get ((props :rows) (props :current-row)) :h 0)]
    (when (> (+ curr-y curr-h) (+ (- (props :scroll)) (props :h)))
      (put props :scroll (- (- (+ curr-y curr-h) (* 0.5 (props :h))))))

    (when (< curr-y (- (props :scroll)))
      (put props :scroll (- (- curr-y
                               (* 0.5 (props :h))))))))
