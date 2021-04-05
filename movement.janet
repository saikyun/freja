(use ./vector_math)
(use jaylib)

(varfn mp
  []
  (let [[x y] (get-mouse-position)]
    [(- x 810) (- y 10)]))

(varfn wasd
  [state dt]

  (def dir @[0 0])

  (when (key-down? :w)
    (update dir 1 - 1))

  (when (key-down? :s)
    (update dir 1 + 1))

  (when (key-down? :a)
    (update dir 0 - 1))

  (when (key-down? :d)
    (update dir 0 + 1))

  (def dir (normalize dir))

  #  (pp dir)

  #(pp (v+ (state :vel) (v* dir dt)))
  (update state :vel v+ (v* dir (* 2000 dt)))
  (update state :cooldown
          (fn [cd]
            (when cd
              (def r (- cd dt))
              (if (< 0 r)
                r
                nil))))

  (def m (-> (mag (state :vel))
             (* 0.9)
#             (max (- max-speed))
#             (min max-speed)
))

  (update state :vel
          (fn [v]
            (-> (normalize v)
                (v* m)))))
