(use ./build/jaylib)
(import ./text_api :prefix "")

(defn measure-text
  [tc text]
  (measure-text-ex (tc :font) text (tc :size) (tc :spacing)))

(defn draw-text
  [tc text pos color]
  (draw-text-ex (tc :font) text pos (tc :size) (tc :spacing) color))

(varfn get-pos-in-text
  [text-data x]
  (def {:conf conf :offset offset} text-data)
  (def {:size font-size :spacing spacing} conf)
  
  (var total-w 0)
  (var last -10000)
  
  (def both (content text-data))  
  
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
