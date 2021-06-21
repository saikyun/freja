(setdyn :doc ````
* a
* b
````)

(import ./vector_math :prefix "")
(import ./freja/events :as e :fresh true)
(import ./freja/input :as i)
(import ./freja/file_handling :as fh)
(import ./freja/new_gap_buffer :as gb)
(import ./freja/render_new_gap_buffer :as render-gb)
(import ./freja/font :prefix "")
(import ./freja/state)
(import ./new_menu_state :as s)
(import ./freja/frp :as frp)
(import ./freja/fonts)
(import ./backwards2 :as b)
(use jaylib)

(def font-size 22)
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
  (default size font-size)
  (default spacing def-spacing)
  (default color text-color)
  (default font s/menu-font)
  (assert pos "need :pos")
  (draw-text-ex font t pos size spacing color))

(varfn measure-text+
  ````
Takes string `text` and returns its size as `[w h]`
By default uses the values set in new_menu,
but with optional keys one can modify these.
````
  [text &keys {:size size :spacing spacing :font font}]
  (default size font-size)
  (default spacing def-spacing)
  (default font s/menu-font)
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
                              font-size))
  #=> [19 22]


  (measure-text+ "ab"
                 :size 40
                 :spacing 20
                 :font (default-load-font-from-memory
                         ".otf"
                         fonts/mplus
                         font-size))
  #=> [52 40]
  #
)

(varfn text+
  [{:text text :pos pos :size size :spacing spacing :text/color color :font font}]
  (default size font-size)
  (default spacing def-spacing)
  (default color text-color)
  (default font s/menu-font)
  (assert pos "need :pos")
  (assert text "need :text")
  (draw-text-ex font text pos size spacing color))

(varfn measure+
  "Same as `measure-text+` but takes a single table/struct."
  [{:text text :size size :spacing spacing :font font}]
  (default size font-size)
  (default spacing def-spacing)
  (default font s/menu-font)
  (measure-text-ex font
                   text
                   size
                   spacing))

(defn draw-file-open-area
  []
  (def text {:text "Open file: "
             :text/color 0xffffffcc
             :pos [6 (render-gb/abs-text-y state/file-open-data -2)]})

  (put state/file-open-data :offset [((measure+ text) 0)
                                     (get-in state/file-open-data [:offset 1])])
  (:draw frp/file-open-area)
  (text+ text))

(def kws {:control "Ctrl"})

(defn kw->string
  [kw]
  (get kws
       kw
       (let [s (string kw)]
         (if (one? (length s))
           (string/ascii-upper s)
           s))))

(defn hotkey->string
  [hk]
  (string/join (map kw->string hk) "+"))

(defn draw-search-area
  []
  (def text {:text "Search: "
             :text/color 0xffffffcc
             :pos [6 (render-gb/abs-text-y state/file-open-data -2)]})

  (put state/search-data :offset [((measure+ text) 0)
                                  (get-in state/file-open-data [:offset 1])])
  (:draw frp/search-area)
  (text+ text))

(var menu @{})


(defn menu-button
  [pos f text]
  [:+ {}
   [:button {:pos pos
             :on-click |(do (put menu :submenu nil)
                          (e/put! state/focus123 :focus frp/text-area)
                          (f (frp/text-area :gb)))}
    text]
   [:button {:pos [(+ (unit 25) (pos 0)) (pos 1)]
             :text/color 0xffffffbb
             :on-click |(do (put menu :submenu nil)
                          (e/put! state/focus123 :focus frp/text-area)
                          (f (frp/text-area :gb)))}
    (let [hotkey (i/get-hotkey ((frp/text-area :gb) :binds) f)]
      (assert hotkey (string "no hotkey for " f))
      (string (hotkey->string hotkey)))]])

(defn menu-root
  [props]
  (defn menu-height
    [n-items]
    (+ (* n-items 20) 10))

  [:+
   {}
   [:button {:pos (urec 2 1)
             :text/color (if (= (props :submenu) :file)
                           0xffffffff
                           0xffffff80)
             :on-click |(do (e/put! state/focus123 :focus menu)
                          (put menu :submenu :file))}
    "File"]
   [:button {:pos (urec 10 1)
             :text/color (if (= (props :submenu) :edit)
                           0xffffffff
                           0xffffff80)
             :on-click |(do (e/put! state/focus123 :focus menu)
                          (put menu :submenu :edit))}
    "Edit"]
   (case (props :submenu)
     :file (let [btns [[i/open-file "Open"]
                       [fh/save-file "Save"]
                       [i/quit "Quit"]
                       #
]]
             [:rec {:rec [0 ((menu :rec) 3)
                          180 (menu-height (length btns))]
                    :bg 0x3E3E3Eff}

              [:+ {}
               ;(seq [i :range [0 (length btns)]
                      :let [[f s] (btns i)
                            rec (urec 2 (+ 7 (* i 5)))]]
                  (menu-button rec f s))]
              #
])

     :edit (let [btns [[i/undo!2 "Undo"]
                       [i/redo! "Redo"]
                       [i/cut! "Cut"]
                       [gb/copy "Copy"]
                       [i/paste! "Paste"]
                       [i/search2 "Search"]
                       #
]]
             [:rec {:rec [(unit 8) ((menu :rec) 3)
                          180 (menu-height (length btns))]
                    :bg 0x3E3E3Eff}
              #   [:button {:pos (urec 10 7)
              #            :on-click |(do (put menu :submenu nil)
              #                          (e/put! state/focus123 :focus frp/text-area)
              #                         (i/undo!2 gb-data))}
              #   "Undo"]

              [:+ {}
               ;(seq [i :range [0 (length btns)]
                      :let [[f s] (btns i)
                            rec (urec 10 (+ 7 (* i 5)))]]
                  (menu-button rec f s))]
              #
]))
   #
])
#


(var render-comp nil)
(var ev-comp nil)

(def def-bg 0x2d2d2dff)

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
                (when (b/in-rec? mp [;pos ;(measure-text+ text :pos pos)])
                  (frp/push-callback! ev noop))

                [:release mp]
                (when (b/in-rec? mp [;pos ;(measure-text+ text :pos pos)])
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

# (draw-text-ex font t pos font-size spacing color)

(varfn menu-rec
  [[x y w h]]
  [x
   y
   (match w
     :max (- (get-screen-width) x)
     w w)
   (match h
     :max (- (get-screen-height) y)
     h h)])

(defn menu-draw
  [menu]
  (draw-rectangle-rec (menu-rec (menu :rec)) def-bg)

  (comment
    (let [t (string (get menu :submenu "<inactive>"))]
      (draw-text+ t :pos [(- ((menu :rec) 2)
                             (unit 1)
                             (first (measure-text+ t)))
                          (unit 1)])))

  (render-comp (menu-root menu))

  #
)

(defn menu-event
  [menu ev]

  (when (frp/mouse-events (first ev))
    (when (b/in-rec? (ev 1) (menu-rec (menu :rec)))
      (frp/push-callback! ev noop))) # done so that nothing below menu reacts

  (match ev
    [:press mp]
    (when (menu :submenu)
      (frp/push-callback! ev |(put menu :submenu nil))))

  (ev-comp (menu-root menu) ev)
  #
)

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

(defn init
  []

  (merge-into menu @{:rec [0 0 :max (unit 7.5)]
                     :submenu nil
                     :draw menu-draw
                     :on-event menu-event})

  (def dependencies
    @{frp/mouse @[frp/text-area frp/search-area frp/file-open-area menu]
      frp/keyboard @[#pp
                     |(-?> (state/focus123 :focus) (:on-event $))]
      frp/text-area @[(fn [{:gb gb}]
                        (-> gb
                            (put :not-changed-timer 0)
                            (put :styled false)))]
      frp/chars @[|(:on-event (state/focus123 :focus) $)]
      state/focus123 @[frp/caret]
      frp/callbacks @[frp/handle-callbacks]})

  (def finally
    @{frp/frame-chan @[frp/render-deps
                       frp/caret
                       |(update-in frp/text-area [:gb :not-changed-timer] + ($ 1))]})

  (def draws @[|(:draw frp/text-area)
               |(get-in frp/text-area [:gb :not-changed-timer])
               |(case (state/focus123 :focus)
                  frp/search-area (draw-search-area)
                  frp/file-open-area (draw-file-open-area))
               |(:draw frp/caret)
               |(:draw menu)])

  (set s/menu-font (default-load-font-from-memory
                     ".otf"
                     fonts/poppins
                     font-size))
  (put frp/deps :deps dependencies)
  (put frp/deps :finally finally)
  (put frp/deps :draws draws)

  (e/put! state/focus123 :focus frp/text-area))
