(varfn render-textarea
  [tc]
  (def {:spacing spacing
        :size font-size} tc)

  (def {:selected selected :text text :after after} text-data)
  
  (def rows (break-up-words text 40 2 280))  
  (let [x 10
        y 35
        w 300
        h (* (length rows) 40)
        roundness 0.3
        segments 9
        diff 2]
    (draw-rectangle-rounded [x y w h] roundness segments (colors :border))
    (draw-rectangle-rounded [(+ x diff)
                             (+ y diff)
                             (- w (* 2 diff))
                             (- h (* 2 diff))]
      roundness
      segments (colors :background)))  
  
  
  (let [[w h] (measure-text tc text)
        [w2 h2] (measure-text tc selected)]
    (var x-ye 0)
    (var y-ye 0)
    
    (let [_ comment]
      (var nof 0)
      
      (var y 0)
#        (print "begin")
      (loop [i :range [0 (length rows)]
             :let [l (+ ;(map (comp length first) (get rows i)))
                   w (+ ;(map |(get $ 1) (get rows i))
                       (- (* spacing (length (get rows i))) spacing))]]
#            (pp rows)
#            (print " " i " " l " " (string/slice text nof l))
        (draw-text-ex font (string/slice text nof (+ nof l)) [30 (+ 30 (* 40 i))] font-size spacing (colors :text))
        (+= nof l)
        (set x-ye 0)
        (set y-ye (* 40 i))
        (when-let [v (get (last (get rows i)) 1)]
          (set x-ye w))))
    
#        (draw-text-ex font text [30 30] font-size spacing (colors :text))
    
    (draw-rectangle-rounded [(+ 30 spacing w)
                             40
                             (+ spacing w2)
                             (- h2 10)]
      0.3
      9
      (colors :selected-text-background))
    
    (draw-text-ex font selected
      [(+ 30 spacing w) 30]
      font-size spacing (colors :selected-text))
    
    (draw-text-ex font
      (string/reverse after)
      [(+ 30 spacing w spacing w2) 30]
      font-size spacing (colors :text))
    
    (draw-line-ex [(+ 30 spacing #w
                     x-ye
                     ) #font-size
                   (+ font-size y-ye)
                   ] [(+ 30 spacing #w
                        x-ye
                        )
                      (+ #font-size
                        (+ font-size y-ye)
                        (* h 0.75))
                      
                      ] 1 (colors :caret))))


(varfn poses-in-text
  [text font-size spacing]
  (var total-w 0)
  (seq [i :range [0 (length text)]
        :let [s2 (string/slice text i (inc i)) 
              [w2 h] (measure-text-ex font s2 font-size spacing)
              w3 (math/ceil (/ w2 2))]]
    (set total-w (+ total-w w2))
    
    (def pos total-w)
    
    (if (not= i (dec (length text)))
      (set total-w (+ total-w spacing)))
    
    [i pos]))

(varfn poses-in-text
  [text font-size spacing]
  (var total-w 0)
  (seq [i :range [0 (length text)]
        :let [s2 (string/slice text i (inc i)) 
              [w2 h] (measure-text-ex font s2 font-size spacing)
              w3 (math/ceil (/ w2 2))]]
    (set total-w (+ total-w w2))
    
    (def pos total-w)
    
    (if (not= i (dec (length text)))
      (set total-w (+ total-w spacing)))
    
    [i pos]))

(varfn break-up-char
  [text spacing max-width]
  (var rows @[@[]])
  (var acc-w 0)
  (var minus 0)
  (each p text
        (def [i w] p)
        (+= acc-w (- w minus))
        (array/push (last rows) [i (- w minus)])
        (when (> acc-w max-width)
          (array/push rows @[])
          (set acc-w 0)
          (set minus (+ (get p 1) spacing))))
  rows)

(defn split-words
  [t]
  (peg/match '(any (+ (capture (some (if-not :s 1)))
                     (capture (* (if-not "\n" :s)))
                     (capture "\n"))) t))

(varfn break-up-words
  [text font-size spacing max-width]
  (var rows @[@[]])
  (var acc-w 0)
  (var minus 0)
  (def split (split-words text))
  (def widths (map |(measure-text-ex font $ font-size spacing) split))
  (loop [i :range [0 (length split)]
         :let [s (get split i)
               w (- (get (get widths i) 0) minus)]]
    (+= acc-w w)
    (+= acc-w spacing)
    (when (> acc-w max-width)
      (array/push rows @[])
      (set acc-w 0)
# (set minus (+ (get p 1) spacing))
      (+= acc-w w)
      (+= acc-w spacing))
    (array/push (last rows) [s w]))
  rows)

(comment
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

