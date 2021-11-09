(import freja/hiccup)
(import freja/events :as e)
(import freja/theme)
(import freja/state)
(import freja-layout/default-tags :as dt)
(import freja/default-hotkeys :prefix "" :export true)
(import freja-jaylib :prefix "" :export true)
(import freja/defonce :prefix "" :export true)
(import ./vector-math :as v :export true)
(import freja/text_rendering :as tr)
(import freja/assets :as a)


(defn measure-text
  [text &keys {:size size
               :font font
               :spacing spacing}]
  (default size 22)
  (default font "EBGaramond")
  (default spacing 1)
  (def font (if (keyword? font)
              (case font
                :monospace "MplusCode"
                :serif "EBGaramond"
                :sans-serif "Poppins"
                (error (string/format ``
font must either be:
* keyword :monospace, :serif or :sans-serif
* string corresponding to a loaded font: %p
``
                                      (keys a/fonts))))
              font))

  (def font (a/font font size))
  (tr/measure-text* font
                    (if (string? text)
                      text
                      (string/format "%p" text))
                    size
                    spacing))

(defn draw-text
  [text pos &keys {:size size
                   :font font
                   :spacing spacing
                   :center center
                   :color color}]
  (default size 22)
  (default font "EBGaramond")
  (default spacing 1)
  (default color 0x000000ee)

  (def font (if (keyword? font)
              (case font
                :monospace "MplusCode"
                :serif "EBGaramond"
                :sans-serif "Poppins"
                (error (string/format ``
font must either be:
* keyword :monospace, :serif or :sans-serif
* string corresponding to a loaded font: %p
``
                                      (keys a/fonts))))
              font))

  (def pos
    (if-not center
      pos
      (let [[x y] pos
            [w h] (measure-text text pos
                                :size size
                                :font font
                                :spacing spacing
                                :center center
                                :color color)]
        [(- x (* 0.5 w))
         (- y (* 0.5 h))])))

  (def font (a/font font size))
  (tr/draw-text* font
                 (if (string? text)
                   text
                   (string/format "%p" text))
                 pos
                 size
                 spacing
                 color))

(defn custom
  [props]
  (def {:render render
        :on-event on-event
        :change change
        :state state} props)

  (-> (dyn :element)
      (dt/add-default-props props)
      (merge-into
        @{:init
          (fn [self _]
            (when state
              (put state :element self))

            (print "focus game")
            (e/put! state/focus :focus self)
            (e/put! state/editor-state (if (props :left) :left-focus :right-focus) true)

            (global-set-key
              [:alt :u]
              (fn [_]
                (e/put! state/focus :focus self)
                (e/put! state/editor-state (if (props :left) :left-focus :right-focus) true))))

          :children []

          :relative-sizing
          (fn [el max-width max-height]
            (-> el
                (put :width (max (or (el :preset-width) max-width)))
                (put :height (or (el :preset-height)
                                 max-height))
                (put :content-width (el :width))
                (put :layout/lines nil))

            el)

          :render (fn [self parent-x parent-y]
                    (put self :focused? (= self (in state/focus :focus)))

                    (put self :render-x parent-x)
                    (put self :render-y parent-y)

                    (render self))

          :on-event (fn [self ev]

                      (defn unfocus
                        []
                        (show-cursor)
                        (put-in state/editor-state
                                [:left-state :editor :gb :blink] 0)
                        (e/put! state/focus :focus
                                (get-in state/editor-state
                                        [:left-state :editor])))

                      (match ev
                        [:key-down :d]
                        (when (key-down? :left-alt)
                          (unfocus))

                        [:key-down :escape]
                        (unfocus)

                        ['(or (= (ev 0) :press)
                              # (= (ev 0) :mouse-move)
) _]
                        (do
                          (e/put! state/focus :focus self)
                          (e/put! state/editor-state (if (props :left) :left-focus :right-focus) true)))

                      (when on-event
                        (on-event ev)))})))

(defn start-game
  ``
props allows following keys:
:render (mandatory) -- called every frame, with &keys :width & :height
                       :width / :height has width / height of the game
:change -- zero args function that is called at the beginning of every frame, if the game is focused. meant to be used to handle input
:on-event -- function that takes a single argument `event`. if present it is called every time an event occurs, e.g. `:key-down`
:state -- table that will be populated with information about the component, e.g. `:element` will be inserted, containing a reference to the element
``
  [props]
  (assert (props :render) "start-game needs :render")

  (e/put! state/editor-state (if (props :left) :left :right)
          (fn [outer-props]
            [:background {:color (if (outer-props :right-focus)
                                   (theme/comp-cols :background)
                                   :blank)}
             [:padding {:all 2}
              [custom props]]])))

(when (dyn :freja/loading-file)
  (start-game {:render (fn render [{:width width :height height}]
                         (draw-rectangle 10 10 (- width 20) (- height 20) :blue))
               :on-event (fn on-event [ev] (pp ev))
               :change (fn [] (print "such change"))
               :state @{}})
  #
)
