(def xo 610)
(def yo 10)

(var step 37)
(var speed 0)
(def acc 0)
(def max-y 685)

(def grid-color [0.8 0.8 1])

(def max-x 690)

(varfn frame
  [dt]
  (def [mx my] (get-mouse-position))
  (+= speed (* acc dt))
  (+= step (* speed dt))
  (loop [y :range [10 max-y step]]
    (draw-line-ex [(+ xo 0) (+ yo y)] [(+ xo 580) (+ yo y)] 2 grid-color))
  (loop [x :range [10 max-x step]]
    (draw-line-ex [(+ xo x) (+ yo 0)] [(+ xo x) (+ yo max-y)] 2 grid-color))


  (def nx (+ xo (* step (math/floor (/ (- mx xo) step)))))

  (def ny (* step (math/floor (/ my step))))

  (draw-rectangle-v [nx ny] [step step] :red)


  (let [y (+ (if-let [{:y y :h h} (last (text-data :rows))]
               (+ y h)
               0)
             16 120)]
    (draw-text* (conf :text) (string (data :latest-res)) [30 y] :blue)
    )
  #
  )

