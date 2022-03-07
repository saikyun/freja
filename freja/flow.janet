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

            #(print "focus game")
            #(e/put! state/focus :focus self)
            #(e/put! state/editor-state (if (props :left) :left-focus :right-focus) true)

            (comment global-set-key
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

                    (defer (rl-pop-matrix)
                      (rl-push-matrix)
                      (render self)))

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

                        ([kind :tab] ({:key-down 1 :key-repeat 1} kind))
                        (when (key-down? :right-control)
                          (swap-top-two-buffers nil))

                        [(_ (or (= (ev 0) :release)
                                # (= (ev 0) :mouse-move)
)) _]
                        (do
                          (def pos (ev 1))

                          (def in?
                            (dt/in-rec? pos
                                        (dyn :offset-x)
                                        (dyn :offset-y)
                                        (self :width)
                                        (self :height)))

                          (when in?
                            (unless (= self (state/focus :focus))
                              (e/put! state/focus :focus self)
                              (e/put! state/editor-state (if (props :left) :left-focus :right-focus)
                                      true)))))

                      (when on-event
                        (on-event self ev)))})))

(defn start-game
  ``
props allows following keys:
:render (mandatory) -- called every frame, with &keys :width & :height
                       :width / :height has width / height of the game
:change -- zero args function that is called at the beginning of every frame, if the game is focused. meant to be used to handle input
:on-event -- function that takes arguments `self` and `event`. if present it is called every time an event occurs, e.g. `:key-down`. `self` is the element doing the rendering.
:state -- table that will be populated with information about the component, e.g. `:element` will be inserted, containing a reference to the element

  Optionally, props can be a function. In this case, that function will be used as `:render` above.
``
  [props]
  (def props
    (if (function? props)
      {:render props}
      props))

  (assert (props :render) "start-game needs :render")

  (def state (get props :state @{}))
  (def props (from-pairs (pairs props)))
  (put props :state state)

  (update state :freja/label |(or $ "Game"))
  (update state :freja/focus |(or $ (fn [{:element el}]
                                      (state/focus! el))))
  (update state :freja/focus? |(or $ (fn [{:element el}] (= el (state/focus :focus)))))

  (e/put! state/editor-state :other
          [(fn [outer-props]
             [:background {:color (if (outer-props :right-focus)
                                    (theme/comp-cols :background)
                                    :blank)}
              [:padding {:all 2}
               [custom props]]])
           state]))

(when (dyn :freja/loading-file)
  (start-game {:render (fn render [{:width width :height height}]
                         (draw-rectangle 10 10 (- width 20) (- height 20) :blue))
               :on-event (fn on-event [self ev] (pp ev))
               :change (fn [] (print "such change"))})
  #
)
