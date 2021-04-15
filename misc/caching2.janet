






(varfn same-rec?
  [[x1 y1 w1 h1]
   [x2 y2 w2 h2]]
  (and (= x1 x2)
       (= y1 y2)
       (= w1 w2)
       (= h1 h2)))

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

(varfn take-mouse-click?
  [rec button]
  (if (and (get-in (dyn :context) [:mouse-button-down? button])
           (point-in-rec? (get-mouse-position) rec))
    (do (put-in (dyn :context) [:mouse-button-down? button] false)
      true)
    false))

(defn select-texture
  [context w h]
  (if-let [t (get-in context [:unused-textures w h])
           (put-in context [:used-textures w h] t)
           (put-in context [:unused-textures w h] nil)
           t]
    (let [t (load-render-texture w h)]
      (put-in context [:used-textures w h] t)
      t)))

(defmacro with-texture
  [dims & body]
  ~(do (select-texture (dyn :context) ,(dims 2) ,(dims 3))
     (begin-texture-mode ((dyn :context) :render-texture))
     (try body
       ([err _]
         (end-texture-mode)
         (throw err)))
     (end-texture-mode)))

(varfn draw-top-bar
  [self data]
  (with-texture [0 0 1024 1024]
    (let [[mx my] (get-mouse-position)]
      (put mr 0 mx)
      (put mr 1 my))
    
    (case open-menu
      :file (file-menu))
    
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
      (def r (draw-text** "Edit" p (if mouse-over 0xffffffff 0xffffff80))))))

(comment
  (def context @{:mouse-button-down? @{0 false
                                       1 false
                                       2 false}
                 :unused-textures @{}
                 :used-textures @{}})
  
  (draw-top-bar)
  )
