(import freja/event/default-subscriptions)
(import freja/event/subscribe)
(import freja/event/jaylib-to-events :as jaylib->events)
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
  (let [ev (input/offset-event-pos ev (dyn :offset-x)
                                   (dyn :offset-y)
                                   :scale (get-in self [:props :scale] 1))]
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
      (state/focus! self))

    (when-let [on-event (get-in self [:props :on-event])]
      (on-event self ev))))

(defn custom
  [props]
  (def {:render render
        :on-event on-event
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
                (put :width (or (el :preset-width) max-width))
                (put :height (or (el :preset-height) max-height))
                (put :content-width (el :width))
                (put :layout/lines nil))

            el)

          :render (fn [self parent-x parent-y]
                    (try
                      (do
                        (def scale (get props :scale 1))
                        (put self :focused? (= self (in state/focus :focus)))

                        (put self :render-x parent-x)
                        (put self :render-y parent-y)

                        (unless (props :render-anywhere)
                          (begin-scissor-mode parent-x parent-y (self :width) (self :height)))

                        (defer (rl-pop-matrix)
                          (rl-push-matrix)
                          (rl-scalef scale scale 1)
                          (render self))

                        (unless (props :render-anywhere)
                          (end-scissor-mode)))
                      ([err fib]
                        (debug/stacktrace fib err))))

          :on-event custom-on-event})))

(defn start-game-f
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

  (defn component
    [outer-props]
    (let [size (props :size)
          scale (get props :scale 1)
          bg (get props :border :blank)]
      [:background {:color bg}
       (if size
         # crazy way to center something
         [:column {}
          [:block {:weight 1}]
          [:row {}
           [:block {:weight 1}]
           [:block {:width (* scale (get-in props [:size 0]))
                    :height (* scale (get-in props [:size 1]))}
            [custom props]]
           [:block {:weight 1}]]
          [:block {:weight 1}]]

         [custom props])]))

  (if (props :new-layer)
    (hiccup/new-layer :game component state)

    (s/put! state/editor-state :other
            [component
             state])))


(defmacro start-game
  ``
  When running from Freja, starts the game in a panel.
  When running from janet or when building an exe, will generate a main-function.
  
  props allows following keys:
  :render (mandatory) -- called every frame, with &keys :width & :height
                         :width / :height has width / height of the game
  :on-event -- function that takes arguments `self` and `event`.
               if present it is called every time an event occurs,
               e.g. `:key-down`.
               `self` is the element doing the rendering.
  :state -- table that will be populated with information about
            the component, e.g. `:element` will be inserted,
            containing a reference to the element
  :render-anywhere -- set to true to disable scissor-mode,
                      i.e. render outside element bounds
  :size -- takes tuple/array `[width height]` where width and height are integers.
           game will be this size in pixels. if not set, game will cover whole panel.
  :scale -- scale factor of the game.
            affects both rendering and events (e.g. :mouse/pos).
  :border -- color of area surrounding the game

  Optionally, props can be a function. In this case, that function will be used as `:render` above.
``
  [props]
  (cond (dyn :freja/web-build)
    ~(upscope
       (defn desktop
         []
         (set-config-flags :msaa-4x-hint)
         (set-target-fps 60))

       (var update-draw-frame nil)
       (var main-fiber nil)
       (defn render-f
         [& _]
         (while (not (window-should-close))
           (begin-drawing)

           (clear-background :white)

           (,jaylib->events/convert (get-frame-time))

           (let [{:regular regular
                  :finally finally}
                 ',state/subscriptions]
             (,subscribe/call-subscribers regular finally))

           (end-drawing))

         (close-window))

       (defn common-startup
         []
         (print "main?")

         (def {:size size
               :scale scale
               :render render} ,props)

         (default scale 1)

         (,default-subscriptions/init)
         #
         (init-window ;(v/v* size scale) "Cross")
         #
         (when (,props :init)
           ((,props :init)))
         #

         (start-game-f (-> (from-pairs (pairs ,props))
                           (put :new-layer true)
                           (put :size nil)))
         #
         #(j/init-audio-device)
         # to facilitate calling from main.c
         (set update-draw-frame |(render-f))
         # XXX
         (setdyn :frame 0)
         # this fiber is used repeatedly by the c code, partly to maintain
         # dynamic variables (as those are per-fiber), but also because reusing
         # a fiber with a function is likely faster than parsing and compiling
         # code each time the game loop performs one iteration
         (print "setting main fiber")
         (set main-fiber
              (fiber/new
                (fn []
                  # XXX: this content only gets used when main.c uses janet_continue
                  (while (not (window-should-close))
                    (print "inner")
                    (render-f)
                    (yield)))
                # important for inheriting existing dynamic variables
                :i))))

    (dyn :freja/loading-file)
    ~(do (when (,props :init)
           ((,props :init)))
       (start-game-f ,props))
    ~(upscope
       (defn main
         [& _]
         (print "main?")

         (def {:size size
               :scale scale
               :render render} ,props)

         (default scale 1)

         (,default-subscriptions/init)

         (init-window ;(v/v* size scale) "Cross")

         (when (,props :init)
           ((,props :init)))

         (start-game-f (-> (from-pairs (pairs ,props))
                           (put :new-layer true)
                           (put :size nil)))

         (set-target-fps 60)

         (var last-mp nil)

         (with-dyns [:offset-x 0 :offset-y 0]
           (while (not (window-should-close))
             (begin-drawing)

             (clear-background :white)

             (,jaylib->events/convert (get-frame-time))

             (let [{:regular regular
                    :finally finally}
                   ',state/subscriptions]
               (,subscribe/call-subscribers regular finally))

             (end-drawing)))

         (close-window)))))

(comment
  (start-game {:render (fn render [{:width width :height height}]
                         (draw-rectangle 0 0 200 200 :blue)
                         (draw-rectangle 100 100 100 100 :black))
               :on-event (fn on-event [self ev] (printf "example on-event: %p" ev))
               :size [200 200]
               :scale 2
               :border [0.3 0 0 1]}))
