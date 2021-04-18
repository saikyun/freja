(defonce pushers @[])
(defonce pullers @[])

(array/clear pushers)
(array/clear pullers)

(defonce click-buffer @[])
(array/clear click-buffer)

(defonce move-buffer @[])
(array/clear move-buffer)

(defonce rerender-buffer @[])
(array/clear rerender-buffer)

(defonce button-click-buffer @[])
(array/clear button-click-buffer)

(defonce rt (load-render-texture 500 500))

(var box @[0 0 50 50])

(var box2 @[0 0 500 80])

(var cb @{:pos [0 0]})
(varfn color-box
  [ev]
  (def [x y] ev)
  (when (in-rec? [(- x 810) (- y 10)] box)
    (array/push button-click-buffer
                |(update box 0 + 10))))

(varfn render-color-box
  []
  (draw-rectangle-rec box :green))

(varfn black-box
  [ev]
  (def [x y] ev)
  (when (in-rec? [(- x 810) (- y 10)] box2)
    (array/push button-click-buffer
                |(print "click black"))))

(varfn render-black-box
  []
  (draw-rectangle-rec box2 [0 0 0]))

(varfn render-to-texture
  [changes]
  (begin-texture-mode rt)
  (clear-background :white)
  (render-black-box)
  (render-color-box)
  (end-texture-mode))

(varfn render
  []
  (draw-texture-pro
    (get-render-texture rt)
    [0 0 500 -500]
    [810 10 500 500]
    [0 0]
    0
    :white))

(varfn render2
  []
  (print "render2"))

(varfn resolve-clicks
  [s]
  (when-let [ev (last s)]
    (ev)))

(varfn pull-signal
  [signal f]
  (f signal)
  (array/clear signal))

(var mouse-down-last-frame false)
(varfn check-click
  []
  (if (mouse-button-down? 0)
    (unless mouse-down-last-frame
      (do (array/push click-buffer (get-mouse-position))
        (set mouse-down-last-frame true)))
    (set mouse-down-last-frame false)))

(var last-mouse-pos nil)
(varfn check-move
  []
  (let [mp (get-mouse-position)]
    (unless (= last-mouse-pos mp)
      (array/clear move-buffer)
      (array/push move-buffer mp)
      (set last-mouse-pos mp))))

(varfn pulls
  []
  (clear-background :white)
  (pull-signal click-buffer
               (fn [s]
                 (loop [ev :in s]
                   #(render-to-texture ev)

                   (black-box ev)
                   (color-box ev))))

  (pull-signal rerender-buffer |(render-to-texture $))

  #  (pull-signal move-buffer |(map render-to-texture $))
  (pull-signal button-click-buffer resolve-clicks)
  #  (pull-signal move-buffer render-to-texture)
)

(varfn draw-frame
  [dt]
  (render))

(varfn each-frame
  [f]
  (while true
    (f)
    (ev/sleep 0.01)))

(defonce pusher (ev/call (fn [] (each-frame (fn []
                                              (check-click)
                                              (check-move))))))


(defonce puller (ev/call |(each-frame |(pulls))))

(comment
  (varonce click-puller nil)

  (-?> click-puller (ev/close))

  (set click-puller (ev/call |(pull-each-frame click-buffer on-click)))

  (varonce input-fiber nil)

  (-?> input-fiber (ev/cancel "canceling"))

  (set input-fiber (ev/call check-click)))
