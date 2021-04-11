(import ./src/state :as s)

(def font-size 16)
(def spacing 1)
(def font (default-load-font "assets/fonts/FiraSans-Regular.ttf" font-size))

(defn unit [v]
  (* v 4))

(var open-menu nil)

(varfn size
  [t]
  (measure-text-ex font
                   t
                   font-size
                   spacing))

(setdyn :layout :vertical)
(var mr @[0 0 1 1])

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
      (put anchor 0 (+ w (in-pos 0)))

      :vertical
      (put anchor 1 (+ h (in-pos 1))))

    anchor))

(varfn file-menu
  []
  (draw-rectangle-rec [0 (unit 6) 100 100] 0x3E3E3Eff)

  (with-dyns [:layout :vertical
              :anchor @[(unit 1) (unit 6)]]

    (def p [(unit 4) (unit 0.5)])
    (def mouse-over (check-collision-recs (get-rec "Save" p) mr))
    (when (and mouse-over (mouse-button-down? 0))
      (save-file gb-data (gb-data :path))
      (set open-menu nil))
    (def r (draw-text** "Save" p (if mouse-over 0xffffffff 0xffffff80)))))

(varfn draw-top-bar
  [self data]
  (let [[mx my] (get-mouse-position)]
    (put mr 0 mx)
    (put mr 1 my))

  (draw-rectangle-rec (self :rec) 0x3E3E3Eff)

  (with-dyns [:layout :horizontal
              :anchor @[(unit 1) (unit 0.5)]]

    (def p [(unit 4) (unit 0.5)])
    (def mouse-over (check-collision-recs (get-rec "File" p) mr))
    (when (and mouse-over (mouse-button-down? 0))
      (set open-menu :file))
    (def r (draw-text** "File" p (if mouse-over 0xffffffff 0xffffff80)))

    (def p [(unit 4) (unit 0.5)])
    (def mouse-over (check-collision-recs (get-rec "Edit" p) mr))
    (def r (draw-text** "Edit" p (if mouse-over 0xffffffff 0xffffff80))))

  (case open-menu
    :file (file-menu)))

(def top-bar {:id :top-bar
              :rec [0 0 (get-screen-width) (unit 6)]
              :draw draw-top-bar})

(-?>> (find-index |(= (top-bar :id) ($ :id)) s/draws)
      (array/remove s/draws))

(array/push s/draws top-bar)
