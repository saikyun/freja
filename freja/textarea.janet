(import freja-layout/default-tags :as dt)
(import ./new_gap_buffer :as gb)
(import ./input :as i)
(import ./collision :as c)
(import freja/theme)
(import freja/frp)
(import ./events :as e)
(import freja/state)
(import ./render_new_gap_buffer :as rgb)

(use profiling/profile)

(defn text-area-on-event
  [self ev]

  (match ev
    [:key-down k]
    (do
      (i/handle-keyboard2
        (self :gb)
        k)
      (put self :event/changed true))
    [:char k]
    (do
      (i/handle-keyboard-char
        (self :gb)
        k)
      (put self :event/changed true))
    [:scroll n mp]
    (when (c/in-rec? mp
                     (i/gb-rec (self :gb)))

      (frp/push-callback! ev (fn []
                               (i/handle-scroll-event (self :gb) n)
                               (put self :event/changed true))))

    ['(i/mouse-events (first ev)) _]
    (i/handle-mouse-event
      (self :gb)
      ev
      (fn [kind f]
        (frp/push-callback! ev (fn []
                                 (f)
                                 #(print "changing focus to: " (self :id))
                                 (e/put! state/focus123 :focus self)
                                 (put (self :gb) :event/changed true)))))))


(defn default-textarea-state
  [&keys {:gap-buffer gap-buffer
          :binds binds
          :extra-binds extra-binds}]
  (default gap-buffer (gb/new-gap-buffer))

  (default binds (table/setproto @{} i/gb-binds))

  (merge-into gap-buffer
              {:binds binds
               :colors theme/colors})

  (when extra-binds
    (put gap-buffer :binds
         (-> (merge-into @{} extra-binds)
             (table/setproto i/gb-binds))))

  @{:gb gap-buffer

    :draw (fn [self]
            (p (get self :id :draw-textarea)
               (do
                 #            (rl-push-matrix)
                 (p :pre-render
                    (rgb/gb-pre-render (self :gb)))

                 #            (rl-push-matrix)

                 #            (rl-load-identity)
                 (p :render-text
                    (rgb/gb-render-text (self :gb)))
                 #            (rl-pop-matrix)
                 #            (rl-pop-matrix)

                 (p :render-cursor
                    (rgb/render-cursor (self :gb))))))

    :on-event (fn [self ev]
                (p :textarea-on-event
                   (text-area-on-event self ev)))})


(defn textarea
  [props & _]
  (def {:state state
        :text/color text/color
        :text/size text/size
        :text/font text/font
        :text/line-height text/line-height
        :text/spacing text/spacing
        :show-line-numbers show-line-numbers} props)

  (default text/size (dyn :text/size 14))
  (default text/font (dyn :text/font "Poppins"))
  (default text/line-height (dyn :text/line-height 1))
  (default text/spacing (dyn :text/spacing 1))
  (default text/color (dyn :text/color 0x000000ff))

  (put-in state [:gb :text/size] text/size)
  (put-in state [:gb :text/font] text/font)
  (put-in state [:gb :text/line-height] text/line-height)
  (put-in state [:gb :text/spacing] text/spacing)
  (put-in state [:gb :text/color] text/color)
  (put-in state [:gb :changed] true)
  (put-in state [:gb :show-line-numbers] show-line-numbers)
  (when show-line-numbers
    (put-in state [:gb :offset] [12 0]))

  (-> (dyn :element)
      (dt/add-default-props props)
      (merge-into
        @{:children []
          :relative-sizing
          (fn [el max-width max-height]
            (print "resizing text area " max-width " " max-height)
            # TODO: something strange happens when width / height is too small
            # try removing 50 then resize to see
            (-> el
                (put :width (max 50 (or (el :preset-width) max-width)))
                (put :height (max (get-in state [:gb :conf :size] 0)
                                  (or (el :preset-height) max-height)))
                (put :content-width (el :width))
                (put :layout/lines nil))

            (def [old-w old-h] (get-in state [:gb :size]))

            (unless (and (= old-w (el :width))
                         (= old-h (el :height)))
              (put-in state [:gb :size]
                      [(math/floor (el :width))
                       (math/floor (el :height))])
              (put-in state [:gb :changed] true)
              (put-in state [:gb :resized] true))

            (print "el: " (el :width) " / " (el :height))

            el)

          :render (fn [self]
                    #                    (print "text area render")
                    (:draw state)
                    #(pp (get-in state [:gb :text]))
)

          :on-event (fn [self ev]
                      #(pp self)
                      #(print "start " (state :id))

                      #(tracev [(dyn :offset-x) (dyn :offset-y)])

                      (defn update-pos
                        [[x y]]
                        [(- x
                            (dyn :offset-x 0))
                         (- y
                            (dyn :offset-y 0))])

                      (def new-ev (if (= (first ev) :scroll)
                                    [(ev 0)
                                     (ev 1)
                                     (update-pos (ev 2))]
                                    [(ev 0)
                                     (update-pos (ev 1))]))

                      #(text-area-on-event state new-ev)
                      (:on-event state new-ev)

                      (def pos (new-ev
                                 (if (= :scroll (first new-ev))
                                   2
                                   1)))

                      (when (dt/in-rec? pos
                                        0
                                        0
                                        (self :width)
                                        (self :height))
                        true))})))
