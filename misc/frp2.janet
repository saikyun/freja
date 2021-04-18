(def mario @{:x 0 :y 0 :vx 0 :vy 0})

(defn physics
  [dt mario]
  (-> mario
      (update :x + (* (mario :vx) dt))
      (update :y + (* (mario :vy) dt))))

(defn walk
  [x mario]
  (update mario :vx * x 0.1))

(defn step
  [[dt dir] mario]
  (walk (dir :x) (physics dt mario)))

(defn render
  [[w h] mario]
  (draw-rectangle 0 0 w h :black)
  (draw-rectangle (mario :x) (mario :y) 35 35 :green))

(def keyboard-arrows @{:x 0 :y 0})
(defn fps
  [fps]
  40)

(defn lift2
  [f a1 a2]
  # ???
)

(defn foldp
  [f rec pushers]
  # ???
)

(def input (lift2 array (fps 25) keyboard-arrows))

(defn main
  []
  (render [500 500]
          (foldp step mario input)))
