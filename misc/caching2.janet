






(varfn same-rec?
  [[x1 y1 w1 h1]
   [x2 y2 w2 h2]]
  (and (= x1 x2)
       (= y1 y2)
       (= w1 w2)
       (= h1 h2)))

(varfn take-mouse-click?
  [rec]
  (if (and ((dyn :context) :check-click)
           (same-rec? rec ((dyn :context) :check-click)))
    true
    (let [nr ((dyn :context) :check-click)
          [x y w h] rec]
      (-> nr
          (put :x x)
          (put :y y)
          (put :w w)
          (put :h h))
      false)))

(varfn draw-top-bar
  [self data]
  
  (let [[mx my] (get-mouse-position)]
    (put mr 0 mx)
    (put mr 1 my))
  
  (when to-draw
    (draw-text-ex font to-draw [800 100] 28 1 0x00ff00ff))
  
  (draw-rectangle-rec (self :rec) 0x3E3E3Eff)
  
  (with-dyns [:layout :horizontal
              :anchor @[(unit 1) (unit 0.5)]]
    (def p [(unit 4) (unit 0.5)])
    (def clicked (take-mouse-click? (get-rec "File" p)))
    (when clicked
      (set open-menu :file))
    
    (def r (draw-text** "File" p (if mouse-over 0xffffffff 0xffffff80)))
    (def p [(unit 4) (unit 0.5)])
    (def r (draw-text** "Edit" p (if mouse-over 0xffffffff 0xffffff80))))
  
  (case open-menu
    :file (file-menu)))



(comment
  (def contexts [])
  
  (draw-all
    (draw-top-bar))
  
  (do
    (with-dyns
      [:context (contexts i)]
      (draw-top-bar)))
  )
