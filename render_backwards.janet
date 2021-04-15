(import ./src/state :as s)
(use spork/test)

(var mouse true)
(def mouse-steal-queue @[])

(varfn steal-mouse-click
  [rec f]
  (array/push mouse-steal-queue f))

(defn hit?
  [rec]
  (< (tracev (rec 0)) ((get-mouse-position) 0)))

(defmacro try-steal-mouse-click
  [rec & body]
  ~(when (and (mouse-button-down? 0)
              (hit? ,rec))
     (steal-mouse-click ,rec (fn [] ,;body))))

(varfn under
  []
  (on-mouse-click
    [0 0 (get-screen-width) 20]
    (print "under clicked"))

  (draw-rectangle-rec [0 0 (get-screen-width) 20] 0x3E3E3Eff))

(varfn on-top
  []
  (on-mouse-click
    [200 0 50 20]
    (print "on-top clicked"))
  (draw-rectangle-rec [200 0 50 20] 0x00ff00ff))

(varfn draw-top-bar
  [self data]
  (array/clear mouse-steal-queue)

  (under)
  (on-top)

  (unless (empty? mouse-steal-queue)
    ((last mouse-steal-queue))
    (pp mouse-steal-queue)))

(def top-bar {:id :top-bar
              :draw draw-top-bar})

(-?>> (find-index |(= (top-bar :id) ($ :id)) s/draws)
      (array/remove s/draws))

(array/push s/draws top-bar)


(comment

  (var texture1 nil)
  (unless texture1
    (set texture1 (load-render-texture (get-screen-width) 20)))

  (var texture2 nil)
  (unless texture2
    (set texture2 (load-render-texture 50 20)))

  (varfn draw-top-bar
    [self data]
    #  (test/timeit (do
    (set mouse true)
    #  (print)

    #                 (begin-texture-mode texture1)
    #                 (clear-background :white)
    (rl-push-matrix)
    (rl-translatef 0 0 0.4)
    (on-top)
    #  (rl-pop-matrix)
    #  (rl-push-matrix)
    (rl-translatef 0 0 -0.2)
    (under)
    (rl-pop-matrix)
    #                 (end-texture-mode)

    #                 (begin-texture-mode texture2)
    #                 (clear-background :white)
    #                 (end-texture-mode)
    (comment
      (draw-texture-pro
        (get-render-texture texture2)
        [0 0 (get-screen-width) -20]
        [0 0 (get-screen-width) 20]
        [0 0]
        0
        :white)

      (draw-texture-pro
        (get-render-texture texture1)
        [0 0 50 -20]
        [200 0 50 20]
        [0 0]
        0
        :white)))
  
  #))
  )
