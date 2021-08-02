(use freja-jaylib)
(import ./theme :as t)
(import ./fonts :as f)
(import ./collision :as c)
(import ./frp)

(def text-color 0xffffffff)
(def def-spacing 1)

(defn noop [&])

(defn unit [v]
  (* v 4))

(defn urec
  [v1 v2]
  [(unit v1) (unit v2)])

(varfn draw-text+
  [t &keys {:pos pos :size size :spacing spacing :text/color color :font font}]
  (default size t/font-size)
  (default spacing def-spacing)
  (default color text-color)
  (default font (f/fonts :default))
  (assert pos "need :pos")
  (draw-text-ex font t pos size spacing color))

(varfn measure-text+
  ````
Takes string `text` and returns its size as `[w h]`
By default uses the values set in new_menu,
but with optional keys one can modify these.
````
  [text &keys {:size size :spacing spacing :font font}]
  (default size t/font-size)
  (default spacing def-spacing)
  (default font (f/fonts :default))
  (measure-text-ex font
                   text
                   size
                   spacing))

(comment
  ### Example usage:
  (measure-text+ "ab")
  #=> [21 22]

  (measure-text+ "ab" :spacing 10)
  #=> [30 22]

  (measure-text+ "ab" :size 40)
  #=> [37 40]

  (measure-text+ "ab" :font (default-load-font-from-memory
                              ".otf"
                              fonts/mplus
                              t/font-size))
  #=> [19 22]


  (measure-text+ "ab"
                 :size 40
                 :spacing 20
                 :font (default-load-font-from-memory
                         ".otf"
                         fonts/mplus
                         t/font-size))
  #=> [52 40]
  #
)

(varfn text+
  [{:text text :pos pos :size size :spacing spacing :text/color color :font font}]
  (default size t/font-size)
  (default spacing def-spacing)
  (default color text-color)
  (default font (f/fonts :default))
  (assert pos "need :pos")
  (assert text "need :text")
  (draw-text-ex font text pos size spacing color))

(varfn measure+
  "Same as `measure-text+` but takes a single table/struct."
  [{:text text :size size :spacing spacing :font font}]
  (default size t/font-size)
  (default spacing def-spacing)
  (default font (f/fonts :default))
  (measure-text-ex font
                   text
                   size
                   spacing))


(def def-bg 0x2d2d2dff)

(var render-comp nil)
(var ev-comp nil)

(def renders
  @{:button (fn [[_ props text]]
              (draw-text+ text ;(kvs props)))
    :rec (fn [o]
           (def {:rec rec :bg bg} (o 1))
           (def cs (drop 2 o))
           (default bg def-bg)
           (draw-rectangle-rec rec bg)
           (loop [c :in cs]
             (render-comp c)))
    :+ (fn [o]
         (def children (drop 2 o))
         (loop [c :in children]
           (render-comp c)))})

(def on-event
  @{:+ (fn [o ev]
         (def children (drop 2 o))
         (loop [c :in children]
           (ev-comp c ev)))
    :button (fn [[_ {:pos pos :on-click on-click} text] ev]
              (match ev
                [:press mp]
                (when (c/in-rec? mp [;pos ;(measure-text+ text :pos pos)])
                  (frp/push-callback! ev noop))

                [:release mp]
                (when (c/in-rec? mp [;pos ;(measure-text+ text :pos pos)])
                  (frp/push-callback! ev on-click))))
    :rec (fn [o ev]
           (def children (drop 2 o))
           (loop [c :in children]
             (ev-comp c ev)))})

(varfn render-comp
  [comp]
  (when comp
    (if-let [f (renders (first comp))]
      (f comp)
      (error (string "No renders defined for " (first comp))))))

(varfn ev-comp
  [comp ev]
  (when comp
    (if-let [f (on-event (first comp))]
      (f comp ev)
      (do comp
        (error (string "No on-event defined for " (first comp)))))))
