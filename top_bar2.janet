(use jaylib)
(import ./src/state :as s)
(import ./src/font :prefix "")

(def font-size 16)
(def spacing 1)
(def font (default-load-font "assets/fonts/FiraSans-Regular.ttf" font-size))

(defn unit [v]
  (* v 4))

(var still-down false)
(var open-menu nil)
(var menu-rec nil)

(varfn size
  [t]
  (measure-text-ex font
                   t
                   font-size
                   spacing))

(setdyn :layout :vertical)
(var mr @[0 0 1 1])

(var to-draw nil)

(varfn get-rec
  [t [x y]]
  (let [[w h] (measure-text-ex font
                               t
                               font-size
                               spacing)
        anchor (dyn :anchor)]
    [(+ x (anchor 0)) (+ y (anchor 1)) w h]))

(varfn draw-text**
  [t in-pos color]
  (def anchor (dyn :anchor))
  (def pos [(+ (in-pos 0) (anchor 0))
            (+ (in-pos 1) (anchor 1))])
  (draw-text-ex font t pos font-size spacing color)
  (let [[w h] (measure-text-ex font
                               t
                               font-size
                               spacing)]

    (case (dyn :layout)
      :horizontal
      (put anchor 0 (+ w (pos 0)))

      :vertical
      (put anchor 1 (+ h (pos 1))))

    anchor))

(comment
  (def top-bar {:id :top-bar
                :rec [0 0 (get-screen-width) (unit 6)]
                :draw draw-top-bar
                :focus? top-bar-focus?})

  (-?>> (find-index |(= (top-bar :id) ($ :id)) s/draws)
        (array/remove s/draws))

  (array/push s/draws top-bar)

  (-?>> (find-index |(= (top-bar :id) ($ :id)) s/focus-checks)
        (array/remove s/focus-checks))

  (array/push s/focus-checks top-bar))
