(import freja/hiccup)
(import freja/theme)
(import freja/state)
(import ./input)
(import freja-layout/default-tags :as dt)
(import freja/default-hotkeys :prefix "" :export true)
(import freja-jaylib :prefix "" :export true)
(import freja/defonce :prefix "" :export true)
(import ./vector-math :as v :export true)
(import freja/text_rendering :as tr)
(import freja/assets :as a)
(import freja/event/subscribe :as s)

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
                    (if (or (buffer? text) (string? text))
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
            [w h] (measure-text text
                                :size size
                                :font font
                                :spacing spacing
                                :color color)]
        [(- x (* 0.5 w))
         (- y (* 0.5 h))])))

  (def font (a/font font size))
  (tr/draw-text* font
                 (if (or (buffer? text) (string? text))
                   text
                   (string/format "%p" text))
                 pos
                 size
                 spacing
                 color))

(defn fill
  [el color]
  (draw-rectangle 0 0 (el :width) (el :height) color))

(defn custom-on-event
  [self ev]
  (let [ev (input/offset-event-pos ev (dyn :offset-x) (dyn :offset-y))]
    (match ev
      # unfocus game panel
      {:key/down :escape}
      (let [top-stack-state (in (last (state/editor-state :stack)) 1)]
        (show-cursor)
        (put-in top-stack-state [:editor :gb :blink] 0)
        (:freja/focus top-stack-state))

      # focus game panel
      ({:mouse/release _
        :mouse/pos p}
        (and (not (= self (state/focus :focus)))
             (dt/in-rec? p 0 0 (self :width) (self :height))))
      (do (state/focus! self)
        (s/put! state/editor-state (if (get-in self [:props :left]) :left-focus :right-focus)
                true)))

    (when-let [on-event (get-in self [:props :on-event])]
      (on-event self ev))))

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
              (put state :element self)))

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

                    (unless (props :render-anywhere)
                      (begin-scissor-mode parent-x parent-y (self :width) (self :height)))

                    (render self)

                    (unless (props :render-anywhere)
                      (end-scissor-mode)))

          :on-event custom-on-event})))

(defn start-game
  ``
  props allows following keys:
  :render (mandatory) -- called every frame, with &keys :width & :height
                         :width / :height has width / height of the game
  :change -- zero args function that is called at the
             beginning of every frame, if the game
             is focused. meant to be used to handle input
  :on-event -- function that takes arguments `self` and `event`.
               if present it is called every time an event occurs,
               e.g. `:key-down`.
               `self` is the element doing the rendering.
  :state -- table that will be populated with information about
            the component, e.g. `:element` will be inserted,
            containing a reference to the element
  :render-anywhere -- set to true to disable scissor-mode,
                      i.e. render outside element bounds

  Optionally, props can be a function. In this case, that function will be used as `:render` above.
``
  [props]
  (def props
    (if (function? props)
      {:render props}
      props))

  (assert (props :render) "start-game needs :render")

  (def state (get props :state @{}))
  # copy the props
  (def props (from-pairs (pairs props)))
  (put props :state state)

  (update state :freja/label |(or $ "Game"))
  (update state :freja/focus |(or $ (fn [{:element el}]
                                      (state/focus! el))))
  (update state :freja/focus? |(or $ (fn [{:element el}] (= el (state/focus :focus)))))

  (s/put! state/editor-state :other
          [(fn [outer-props]
             [:background {:color (if (outer-props :right-focus)
                                    (theme/comp-cols :background)
                                    :blank)}
              [:padding {:all 2}
               [custom props]]])
           state]))

(when (dyn :freja/loading-file)
  (start-game {:render (fn render [{:width width :height height}]
                         (draw-rectangle -20 0 (- width 20) (- height 20) :blue))
               :on-event (fn on-event [self ev] (printf "example on-event: %p" ev))
               :change (fn [] (printf "such change"))})
  #
)
