(import freja/frp)
(import freja/theme)

(defonce popup @{})

(var y 300)
(var acc 0)

(merge-into
  popup
  @{:draw (fn [self ev])

    :on-event
    (fn [self ev]
      #(pp ev)
      #

      (when (= :press (first ev))
        (pp (first ev))
        (-= y 100))

      (+= acc (* 10 (ev 1)))
      (+= y acc)

      (when (> y (- 800 220))
        (set acc (- acc)))

      (def w 300)
      (def h 100)
      (def offset [300 0])

      (draw-rectangle-rec [(+ (offset 0)
                              (- (* 0.5 (get-screen-width))
                                 (* 0.5 w)))
                           (+ (offset 1)
                              (- (* 0.5 (get-screen-height))
                                 (* 0.5 h)))
                           w
                           h]
                          (theme/comp-cols :background))

      #      (draw-rectangle-rec [600 y 180 120] (:background theme/comp-cols))
)})

#(frp/subscribe! frp/mouse popup)
(frp/subscribe-finally! frp/frame-chan popup)

#(frp/unsubscribe-finally! frp/frame-chan popup)

