# <unnamed> is something which can change over time
# it is implemented as a fiber, which when resumed
# will return a 2 element array or tuple.
# the first element will be wether <unnamed> has changed
# the second element will be the value of <unnamed>

# there are various operations one can do to <unnamed>
# one can chain multiple <unnamed> together
# to create e.g. a cause and effect
# such as "mario moves, so we render him in a new position"

(use spork/test)

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

(def state {:buttons [{:hitbox @[810 30 100 100]
                       :render |(draw-rectangle-rec ($ :hitbox) :red)
                       :f |(print "red!")}
                      {:hitbox @[860 0 100 100]
                       :render |(draw-rectangle-rec ($ :hitbox) :blue)
                       :f |(print "blue!")}]})

(defn render
  [[w h] buttons]
  (rl-push-matrix)

  (rl-translatef 810 0 0)
  (draw-rectangle 0 0 w h :black)
  (rl-pop-matrix)

  (loop [b :in buttons]
    ((b :render) b))
  #  (draw-rectangle (mario :x) (mario :y) 35 35 :red)
)

(defn hit-me?
  [pos rec]
  (in-rec? pos rec))

(varfn keyboard-arrows
  ``
An <unnamed> for handling arrow keys.
Whenever arrow keys are hit, it will yield [true @{:x x :y y}]
x = -1 if left key is held, x = -1 if right key is held
y = -1 if up key is held,   y =  1 if down key is held

If no arrow keys are hit since last resume call,
will return [false @{:x x :y y}] instead.
``
  []
  (fiber/new
    (fn []
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

        (yield res)))))


(varfn mouse-click
  ``
An <unnamed> for handling mouse clicks
``
  []
  (fiber/new
    (fn []
      (def buttons-down @{})
      (def res @[false [0 0]])

      (defn listen
        [button]

        (if (mouse-button-down? button)
          (do
            (unless (buttons-down button)
              (put res 0 true)
              (put res 1 (get-mouse-position)))

            (put buttons-down button true))
          (put buttons-down button false)))

      (while true
        (put res 0 false)

        (listen 0)

        (yield res)))))


(defn fps
  ``
An <unnamed> that always returns [true 40]
``
  [fps]
  (fiber/new (fn [] (while true (yield [true 40])))))

(defn lift
  ``
Combines the result of multiple <unnamed>
and returns a new <unnamed>
An analogue would be running (f 1 2)
Where 1 and 2 are the value of two <unnamed>
``
  [f & args]
  (fiber/new (fn []
               (def or2 |(or $0 $1))

               (while true
                 (def res (map resume args))
                 (yield [(reduce or2 false (map first res))
                         (f ;(map 1 res))])))))

(comment
  (resume (lift array (fps 20) (fps 30)))
  #=> (true @[40 40])
)

(defn foldp
  ``
Takes a function f, a value v and an <unnamed> pusher.
Whenever the <unnamed> is changed, calls f on the value of the <unnamed> and on v.
If <unnamed> is not changed, returns v.
Returns a <unnamed>.

Most useful when v is a table / array, and f mutates it.
``
  [f v pusher]
  (fiber/new
    (fn []
      (while true
        (def [changed res] (resume pusher))
        (yield [changed
                (if changed
                  (f res v)
                  v)])))))

(comment
  (resume (foldp + 40 (fps 40)))
  #=> (true 80)

  (resume (foldp + 40 (fiber/new (fn [] [false 40]))))
  #=> (false 40)
)

(defn applyp
  ``
Returns an <unnamed> that will call f on the value of <unnamed> pusher
whenever pusher is changed.
``
  [f pusher]
  (fiber/new
    (fn []
      (while true
        (def [changed res] (resume pusher))
        (yield [changed
                (if changed
                  (f res)
                  nil)])))))

# combine the two <unnamed> into a single <unnamed>
# whenever resume is called on input
# it will return [changed [40 @{:x x :y y}]]
(def input (lift array (fps 25) (keyboard-arrows)))

(comment
  (resume input)
  #=> [true @[40 @{:x 0 :y 0}]]
)

# an unnamed which will call render
# whenever input is changed
# foldp will return an <unnamed> which calls
# step on mario and the value of input
# whenever input is changed
(def main (applyp (partial render [500 500])
                  #(foldp step mario input)

                  (fiber/new (fn []
                               (yield [true (state :buttons)])
                               (while true
                                 (yield [true (state :buttons)]))))))

(resume main)

(def other-thing (applyp pp (mouse-click)))

# is run every frame
# but will only rerender if something is changed
# but since fps always returns a true change
# it will run render every frame

(varfn draw-frame
  [dt]
  (resume main)
  (let [recs (state :buttons)]
    (->> (foldp (fn [pos recs]
                  (filter |(hit-me? pos ($ :hitbox)) recs))
                recs
                (mouse-click))
         (applyp last) # we want the one rendered last
         (applyp |(-?> $ (get :f) apply))
         resume)))
