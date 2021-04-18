(def mario @{:x 0 :y 0 :vx 0 :vy 0})

(defn physics
  [dt mario]
  (-> mario
      (update :x + (* (mario :vx) dt))
      (update :y + (* (mario :vy) dt))))

(defn walk
  [x mario]
  (put mario :vx (* x 0.1)))

(defn up-down
  [y mario]
  (put mario :vy (* y 0.1)))

(defn step
  [[dt dir] mario]
  (walk (dir :x) (physics dt mario))
  (up-down (dir :y) (physics dt mario)))

(defn render
  [[w h] mario]
  (rl-push-matrix)

  (rl-translatef 810 0 0)

  (try
    (do
      (draw-rectangle 0 0 w h :black)
      (draw-rectangle (mario :x) (mario :y) 35 35 :green))
    ([err _]
      (pp err)))
  (rl-pop-matrix))

(varfn keyboard-arrows
  []
  (def keyboard-arrows @{:x 0 :y 0})

  (def keys-down @{})

  (def res @[false keyboard-arrows])

  (defn listen
    [key k v]

    (if (key-down? key)
      (do
        (unless (keys-down key)
          (put res 0 true)
          (update keyboard-arrows k + v))

        (put keys-down key true))
      (do
        (when (keys-down key)
          (put res 0 true)
          (update keyboard-arrows k - v))

        (put keys-down key false))))

  (while true
    (put res 0 false)

    (listen :left :x -1)
    (listen :right :x 1)
    (listen :up :y -1)
    (listen :down :y 1)

    (yield res)))

(defn fps
  [fps]
  (fiber/new (fn [] (while true (yield [true 40])))))

(defn lift
  [f & args]
  (fiber/new (fn []
               (def or2 |(or $0 $1))

               (while true
                 (def res (map resume args))
                 (yield [(reduce or2 false (map first res))
                         (f ;(map 1 res))])))))

(defn foldp
  [f rec pusher]
  (fiber/new
    (fn []
      (while true
        (def [changed res] (resume pusher))
        (yield [changed
                (if changed
                  (f res rec)
                  rec)])))))

(defn applyp
  [f pusher]
  (fiber/new
    (fn []
      (while true
        (def [changed res] (resume pusher))
        (yield [changed
                (if changed
                  (f res)
                  nil)])))))


(def kb-arr (fiber/new keyboard-arrows))

(def input (lift array (fps 25) kb-arr))

(def main (applyp (partial render [500 500])
                  (foldp step mario input)))

(varfn draw-frame
  [dt]
  (resume main))
