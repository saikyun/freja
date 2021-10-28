(import freja-layout/default-tags :as dt)
(import freja/state)
(import freja/events :as e)
(import freja/assets :as a)
(import freja/text_rendering :as tr)

(defn draw-text
  [text pos &keys {:size size
                   :font font
                   :spacing spacing
                   :color color}]
  (default size 22)
  (default font "EBGaramond")
  (default spacing 1)
  (default color 0x000000ee)
  (keys a/fonts)
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
        :on-event on-event} props)

  (-> (dyn :element)
      (dt/add-default-props props)
      (merge-into
        @{:children []

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
                        (put-in state/editor-state
                                [:left-state :editor :gb :blink] 0)
                        (e/put! state/focus :focus
                                (get-in state/editor-state
                                        [:left-state :editor])))

                      (match ev
                        [:key-down :escape]
                        (unfocus)

                        ['(or (= (ev 0) :press)
                              (= (ev 0) :mouse-move)) _]
                        (do
                          (e/put! state/focus :focus self)
                          (e/put! state/editor-state :right-focus true)))

                      (when on-event
                        (on-event ev)))})))
