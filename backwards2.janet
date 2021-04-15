# two kinds of renders

(import jaylib :as j)
(import ./src/state :as s)

(var mouse-stolen false)

(defn mouse-button-down?
  [k]
  (def context (dyn :context))
  (when (or (and context (context :captured-mouse))
            (not mouse-stolen))
    (j/mouse-button-down? k)))

# regular function renders
(defn just-render-it
  []
  (when (mouse-button-down? 0)
    (print "clicked blue thingy"))
  (j/draw-rectangle-rec [0 0 2000 20] :blue))

# function renders with context (env?)
(def complex
  (let [rec @[0 0 100 100]]
    @{:context @{:capture-mouse rec}
      :render (fn []
                (when (mouse-button-down? 0)
                  (print "hit me"))

                (j/draw-rectangle-rec [0 0 100 100] :green))}))

(varfn in-rec?
  [[px py] [x y w h]]
  (and
    (>= px x)
    (<= px (+ x w))
    (>= py y)
    (<= py (+ y h))))

(defn render-all
  [fs]
  ## reset mouse clicked
  (set mouse-stolen false)

  # loop through all things that might capture mouse
  (loop [i :down-to [(dec (length fs)) 0]
         :let [f (fs i)]
         :when (table? f)
         :let [{:context context} f]
         :when context]
    (when-let [r (context :capture-mouse)]
      (when (and (not mouse-stolen)
                 (r (j/get-mouse-position))) # check if mouse gets captured
        (put context :captured-mouse true)
        (set mouse-stolen true))))

  # render all the things
  (loop [f :in fs]
    (if (table? f)
      (with-dyns [:context (get f :context)]
        ((f :render)))
      (f))))

(comment
  (def top-bar {:id :top-bar
                :draw render-all})

  (-?>> (find-index |(= (top-bar :id) ($ :id)) s/draws)
        (array/remove s/draws))

  (array/push s/draws top-bar))
