(def xo 605)

(def owl-state @{:pos @[50 250]
                 :vel @[0 0]})

(varfn frame
  [dt]
  (draw-text (conf :text) (string (data :latest-res)) [605 660] :blue)

  (draw-rectangle-rec [xo 7 585 645] :white)
  (draw-rectangle-rounded-lines [xo 7 585 645] 0.015 9 2 (colors :border))

  (update-in owl-state [:vel 1] + 0.25)
  (update-in owl-state [:pos 1] + (get-in owl-state [:vel 1]))
  (draw-rectangle-pro [(+ xo (get-in owl-state [:pos 0])) (get-in owl-state [:pos 1]) 50 50] [25 25] (* 4 (get-in owl-state [:vel 1])) :red)
  #(draw-rectangle-rounded [(+ xo 10) (get-in owl-state [:pos 1]) 50 50] 0.5 9 :red)

  (draw-text (text-data :conf)
             (string (get-in owl-state [:vel 1]))
             [xo 5]
             :red)
  #
  )


(put game-binds :space nil)

(put pressed-game-binds :space |(do (print "huh?") (update-in owl-state [:vel 1] - 5)))
