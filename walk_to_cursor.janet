(use ./movement)
(use ./vector_math)

(defn new-state
  [x y]
  @{:pos @[x y]
    :vel @[0 0]
    :acc @[0.1 0.1]
    :scale 12
    :color [(math/random) (math/random) (math/random)]})

(def state (new-state 250 250))
(put state :max-speed 200)

(def diff 200)
(def min-dist 50)
(def others (seq [i :range [0 10]
                  :let [x (+ min-dist (* (- diff min-dist) (math/random)))
                        x (if (> (math/random) 0.5)
                            (- x) x)
                        x (+ 250 x)
                        y (+ min-dist (* (- diff min-dist) (math/random)))
                        y (if (> (math/random) 0.5)
                            (- y) y)
                        y (+ 250 y)]]
              (new-state x y)))

(pp (data :focus))

(var bullets @[])

(varfn v2->ma
  [v2]
  (let [[x y] v2
        m (mag v2)]
    @{:mag m
      :angle (math/atan2 y x)}))

(varfn ma->v2
  [mag angle]
  @[(* mag (math/cos angle))
    (* mag (math/sin angle))])

(varfn shotgun-bullet
  [state]
  (let [b (new-state ;(state :pos))
        vel (-> (v- (state :pos)
                    (mp))
                normalize
                (v* -1800))]
    (-> b
        (put :scale 0.5)
        (put :vel vel)
        (put :color [0.2 0.2 0.2])
        (put :retardation 0.9))))

(varfn update-angle
  [v2 & args]
  (let [[x y] v2
        m (mag v2)
        angle ((first args) (math/atan2 y x)
                            ;(array/slice args 1))]
    (ma->v2 m angle)))

(varfn update-mag
  [v2 & args]
  (let [[x y] v2
        m ((first args) (mag v2)
                        ;(array/slice args 1))
        angle (math/atan2 y x)]
    (ma->v2 m angle)))

(varfn shoot
  [state]
  (unless (state :cooldown)
    (put state :cooldown 0.1)
    (loop [i :range [-5 5]
           :let [b (shotgun-bullet state)]]
      (update b :vel update-angle +
              (* (+ (* i 0.02) (* 0.3 (math/random)))))
      (update b :vel update-mag + (* 1300 (math/random)))
      (put b :scale (+ 2.3 (* 2.1 (math/random))))
      (update state :vel v+ (v* (b :vel) -0.02))
      (array/push bullets b))))

(varfn shoot-one
  [state]
  (unless (state :cooldown)
    (put state :cooldown 0.1)
    (loop [i :range [0 1]
           :let [b (shotgun-bullet state)]]
      (update b :vel update-angle +
              (* (+ (* i 0.02) (* 0.1 (math/random)))))
      (update b :vel update-mag + (* 1000 (math/random)))
      (put b :scale (+ 0.3 (* 0.15 (math/random))))
      (update state :vel v+ (v* (b :vel) -0.05))
      (array/push bullets b))))

(varfn frame
  [dt]
  (clear-background :white)

  (def [mx my] (mp))

  (if (> mx 0)
    (put data :focus :game)
    (when (= :game (data :focus))
      (put data :focus :main)))

  (when (mouse-button-pressed? 0)
    (shoot state))

  (when (mouse-button-pressed? 1)
    (shoot-one state))

  (wasd state dt)

  (update state :pos v+ (v* (state :vel) dt))

  (def to-remove @[])
  (loop [i :range [0 (length bullets)]
         :let [b (bullets i)]]
    (update b :pos v+ (v* (b :vel) dt))
    (update b :vel v* (* (- 1 (/ (b :scale) 50)) (b :retardation)))

    (update b :scale - (* 0.00008 (mag (b :vel))))

    (loop [o :in others]
      (when (>= (+ (o :scale) (b :scale))
                (mag (v- (o :pos) (b :pos))))
        (put b :dead true)
        (update o :vel v+ (v* (b :vel) 0.05))
        (break)))

    (when (or (b :dead)
              (<= (mag (b :vel)) 300))
      (array/push to-remove i)))

  (var diff 0)
  (loop [i :in to-remove]
    (array/remove bullets (- i diff))
    (++ diff))

  (def {:pos pos :scale scale} state)

  (loop [o :in others]
    #(put-in o [:acc 0] (* 500 (- (math/random) 0.5)))
    #oa(put-in o [:acc 1] (* 700 (- (math/random) 0.5)))

    (update o :vel v+ (v* (o :acc) dt))

    (update o :pos v+ (v* (o :vel) dt)))

  #  (draw-line-ex
  #    pos
  #    (mp)
  #    3
  #    :green)

  (draw-circle-v pos (* scale 1) :black)

  (loop [b :in bullets
         :let [{:pos pos :scale scale :color color} b]]
    (draw-circle-v pos (* scale 1) color))

  (loop [o :in others
         :let [{:pos pos :scale scale :color color} o]]
    (draw-circle-v pos (* scale 1) color)))
