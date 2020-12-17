(use ./build/jaylib)
(import ./text_rendering :prefix "")

(varfn render-textfield
  [conf text-data]
  (def {:text text-conf
        :colors colors} conf)
  
  (def {:spacing spacing
        :size font-size} text-conf)
  
  (def {:text text
        :selected selected
        :after after} text-data)
  
  (let [x 10
        y 35
        w 300
        h 40
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
  
  
  (let [[w h]   (measure-text text-conf text)
        [w2 h2] (measure-text text-conf selected)]
    
    (draw-text text-conf text [30 30] (colors :text))
    
    (draw-rectangle-rounded [(+ 30 spacing w)
                             40
                             (+ spacing w2)
                             (- h2 10)]
      0.3
      9
      (colors :selected-text-background))
    
    (draw-text text-conf selected [(+ 30 spacing w) 30] (colors :selected-text))
    
    (draw-text text-conf (string/reverse after) [(+ 30 spacing w spacing w2) 30] (colors :text))
    
    (draw-line-ex
      [(+ 30 spacing w) font-size]
      [(+ 30 spacing w) (+ font-size (* h 0.75))]
      1
      (colors :caret))))
