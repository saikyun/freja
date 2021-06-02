(import ./vector_math :prefix "")

(def font-size 16)
(def text-color 0xffffffff)
(def def-spacing 1)
(def menu-font (default-load-font "assets/fonts/FiraSans-Regular.ttf" font-size))

(defn noop [&])

(defn unit [v]
  (* v 4))

(defn urec
  [v1 v2]
  [(unit v1) (unit v2)])

(varfn draw-text+
  [t &keys {:pos pos :size size :spacing spacing :color color :font font}]
  (default size font-size)
  (default spacing def-spacing)
  (default color text-color)
  (default font menu-font)
  (draw-text-ex font t pos size spacing color))

(varfn draw-text+
  [t &keys {:pos pos :size size :spacing spacing :text/color color :font font}]
  (default size font-size)
  (default spacing def-spacing)
  (default color text-color)
  (default font menu-font)
  (draw-text-ex font t pos size spacing color))

(varfn measure-text+
  [t &keys {:size size :spacing spacing :font font}]
  (default size font-size)
  (default spacing def-spacing)
  (default font menu-font)
  (measure-text-ex font
                   t
                   font-size
                   spacing))

(var menu @{})

(defn menu-root
  [props]
  [:+
   {}
   [:button {:pos (urec 2 1)
             :text/color (if (= (props :submenu) :file)
                           0xffffffff
                           0xffffff80)
             :on-click |(put menu :submenu :file)} "File"]
   (case (props :submenu)
     :file [:rec {:rec [0 ((menu :rec) 3) 200 200]}
            [:button {:pos (urec 2 7)
                      :on-click |(do (print "lul")
                                   (put menu :submenu nil)
                                   (open-file nil))}
             "Open"]])]
  #
)

(var render-comp nil)
(var ev-comp nil)

(def def-bg 0x3E3E3Eff)

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
                (when (in-rec? mp [;pos ;(measure-text+ text :pos pos)])
                  (push-callback! ev noop))

                [:release mp]
                (when (in-rec? mp [;pos ;(measure-text+ text :pos pos)])
                  (push-callback! ev on-click))))
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
      (do (tracev comp)
        (error (string "No on-event defined for " (first comp)))))))

# (draw-text-ex font t pos font-size spacing color)

(defn menu-draw
  [menu]
  (draw-rectangle-rec (menu :rec) def-bg)

  (let [t (string (get menu :submenu "<inactive>"))]
    (draw-text+ t :pos [(- ((menu :rec) 2)
                           (unit 1)
                           (first (measure-text+ t)))
                        (unit 1)]))

  (render-comp (menu-root menu))

  #
)

(defn menu-event
  [menu ev]

  (when (mouse-events (first ev))
    (when (in-rec? (ev 1) (menu :rec))
      (push-callback! ev noop))) # done so that nothing below menu reacts

  (match ev
    [:press mp]
    (when (menu :submenu)
      (push-callback! ev |(put menu :submenu nil))))

  (ev-comp (menu-root menu) ev)
  #
)

(merge-into menu @{:rec [0 0 (get-screen-width) (unit 6)]
                   :submenu :file
                   :draw menu-draw
                   :on-event menu-event})
#
#
#
#
#
#
#
#
#
#
#
#
# stuff to add menu to deps


(import ./misc/frp4 :prefix "")

(def dependencies
  @{mouse @[text-area search-area file-open-area menu]
    keyboard @[#pp
               |(:on-event (focus :focus) $)]
    chars @[|(:on-event (focus :focus) $)]
    focus @[|(print (($ :focus) :id)) caret]
    callbacks @[handle-callbacks]})

(put deps :deps dependencies)


(def draws @[|(:draw text-area)
             |(case (focus :focus)
                search-area (:draw search-area)
                file-open-area (:draw file-open-area))
             |(:draw caret)
             |(:draw menu)])


(put deps :draws draws)
