(defmacro defonce
  "Define a value once."
  [name & more]
  (when (nil? (dyn name))
    ~(def ,name ,;more)))

(defonce state @{:x 0
                 :y 0
                 :vel-y 0
                 :vel-x 0
                 :angle-vel 0
                 :angle 0})

(put state :y 0)
(put state :angle 130)
(put state :angle-vel 2)
(put state :x 250)
#put state :vel-y 0)
(put state :vel-x 0)

(defonce tex (load-render-texture 40 20))

(def font (get-in gb-data [:conf :font]))

(varfn frame
  [dt]
  (def {:y y :x x :angle angle} state)

  (def sin
    (math/sin (* math/pi (/ angle 180))))

  (update state :vel-y + (* dt 200))
  (update state :vel-x * 0.99)
  (update state :vel-y * 0.99)
  (update state :angle |(mod (+ $ (* (state :angle-vel) dt 200)) 360))
  (update state :y + (* dt 10 (state :vel-y)))
  (update state :x + (* dt (state :vel-x)))
  (update state :angle-vel math/pow 0.9)

  (when (> (state :y) (- 500 20))
    (update state :vel-y -)
    (def vel (+ (state :vel-y) (state :vel-x)))
    (put state :vel-y (* vel (- 2 (+ (* 0.2 0.125 sin) 1))))
    (put state :vel-x (* vel (* 2 sin)))
    (update state :angle-vel + 1)
    (update state :angle-vel * 1.25)
    (update state :vel-y + (* 0.05 (state :angle-vel)))
    (put state :y (- 500 20)))

  (when (neg? (state :y))
    (update state :vel-y |(* 0.8 (- $)))
    (put state :y 0))

  (when (neg? (state :x))
    (update state :vel-x |(* 0.8 (- $)))
    (put state :x 0))

  (when (> (state :x) 480)
    (update state :vel-x |(* 0.8 (- $)))
    (put state :x 480))

  (def {:y y :x x :angle angle} state)
  (clear-background :white)
  (end-texture-mode)

  (begin-texture-mode tex)
  (clear-background :white)
  (draw-ellipse 20 10 20 10 :blue)
  (end-texture-mode)

  (begin-texture-mode texture)

#  (draw-text-ex
#    font
#    (string sin)
#    [10 5] 14 1 :black)

  (draw-texture-pro (get-render-texture tex)
                    [0 0 40 (- 20)]
                    [x (math/round y)
                     #0 0
                     40 20]
                    [20 10]
                    angle
                    :white))
