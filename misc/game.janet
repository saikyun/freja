(def aest @{:str 0.7
            :dir -
            :limits [0.6 0.8]
            :speed 5})

(def aest2 @{:str 0.5
             :dir -
             :limits [0.5 0.7]
             :speed 0.01})

(varfn frame
  [dt]
  (draw-text (conf :text) (string (data :latest-res)) [605 660] :blue)

  (defn refresh-color
    [aest]
    (update aest :str (aest :dir) (* dt (aest :speed))))

  (defn update-aest
    [aest]
    (refresh-color aest)
    (def [mi ma] [-3.14 3.14])
    (cond (> mi (aest :str)) (do (put aest :dir +) (refresh-color aest))
          (< ma (aest :str)) (do (put aest :dir -) (refresh-color aest))))

  (update-aest aest)
  (update-aest aest2)

  (draw-rectangle-rec [605 10 100 100] :white)
  (draw-rectangle-gradient-v 605 7 585 645
                             (map |(* $ (+ (get-in aest [:limits 0])
                                           (* (- (get-in aest [:limits 1])
                                                 (get-in aest [:limits 0]))
                                              (* (/ 1 3.14)  (* 0.5 (+ 3.14 (math/sin (aest :str)))))))) [1 1 0])
                             (map |(math/sin (* $ (aest2 :str))) [1 0 1]))

  (draw-text (text-data :conf) (string (aest :str) " - "
                                       (math/sin (aest :str)) " - "
                                       (+ (get-in aest [:limits 0])
                                          (*
                                           (- (get-in aest [:limits 1])
                                              (get-in aest [:limits 0]))
                                           (math/sin (aest :str)))))

             [615 10] :white)

  (draw-rectangle-rounded-lines [605 7 585 645] 0.015 9 2 (colors :border))

  #
  )
