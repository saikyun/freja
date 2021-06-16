(import ./freja/state :as s)

(def font-size 10)
(def diff 2)
(def spacing 1)
(def font (default-load-font "assets/fonts/Monaco.ttf" font-size))
(def font2 (default-load-font "assets/fonts/Monaco.ttf" (+ font-size diff)))

(varfn draw-text**
  [t pos color]
  (draw-text-ex font t pos font-size spacing color))


(varfn draw-text**2
  [t pos scale color]
  (draw-text-ex font2 t pos (* scale (+ font-size diff)) spacing color))

(varfn draw
  [self data]
  (var x 810)
  (loop [c :in ["(" "+" " " "1" " " "1" ")"]]
    (draw-text**2 c [(+ x) 9] 1 0x00000080)
    #    (draw-text**2 c [(- x 0.5) 9] 1 0x00000080)
    (draw-text** c [x 10] 0x000000ff)
    (+= x 7))


  (var x 810)
  (loop [c :in ["(" "+" " " "1" " " "1" ")"]]
    (draw-text** c [x 30] 0x000000ff)
    (+= x 7))
  )

(def top-bar @{:id :yeah
               :draw draw})


(-?>> (find-index |(= (top-bar :id) ($ :id)) s/draws)
      (array/remove s/draws))

(array/push s/draws top-bar)
