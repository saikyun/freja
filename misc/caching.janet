(defn invalidate!
  "Invalidates the cache, i.e. forces it to rerender next `cached-render` call."
  [& [c]]
  (default c (dyn :cache))
  (put c :valid false))

(defn update!
  "Like update, but also runs invalidate!"
  [& args]
  (update ;args)
  (invalidate! (dyn :cache)))

(defn put!
  "Like put, but also runs invalidate!"
  [& args]
  (put ;args)
  (invalidate! (dyn :cache)))

(defmacro cached-render
  "Only runs body when (dyn :cache) is invalidated, or the dimensions are changed."
  [[x y w h] & body]
  ~(do (def dims ((dyn :cache) :dims))
     
     (put dims :x x)
     (put dims :y y)       
     
     (def dimensions-changed (or (not= w (dims :w))                  
                                 (not= h (dims :h))))
     (when (or (not ((dyn :cache) :valid))
               dimensions-changed)
       (when dimensions-changed
         (put ((dyn :cache) :texture)
              (load-render-texture w h)))
       
       (begin-texture-mode ((dyn :cache) :texture))
       ,;body
       (end-texture-mode)
       (put (dyn :cache) :valid true)
       (put dims :w w)
       (put dims :h h))
     
     (draw-texture-pro ((dyn :cache) :texture)
                       [0 0 (dims :w) (dims :h)]
                       dims
                       0
                       :white)))

(def state @{:x 10})

(defn window
  []
  (when trigger
    (update! state :x inc))
  
  (when other-trigger
    (put! state :y (* 2 (state :x))))
  
  (when third-trigger
    (print "wat")
    (invalidate!))
  
  (handle-keyboard-stuff)
  
  (cached-render
    [(state :x) (state :y) 100 20] # dimensions of the render texture
    (clear-background :white)
    (draw-rectangle-rec [0
                         0
                         100
                         20]
                        :green)))
