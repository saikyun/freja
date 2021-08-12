# hitting ctrl+l
(import freja-layout/jaylib-tags :as jt)
(import freja-layout/sizing/relative :as rs)
(import freja-layout/default-tags :as dt)
(import freja/hiccup :as h)
(import freja/events :as e)
(import freja/input :as i)
(import freja/file-handling :as fh)
(import freja/theme :as t)


(import freja/new_gap_buffer :as gb)
(import freja/frp)
(import freja/collision :as c)
(import freja/state)
(use freja/defonce)
(use freja-jaylib)

(setdyn :pretty-format "%.40M")

(defonce sh-state @{})

(defonce text-area-state (frp/default-text-area))
(defonce text-area-state2 (frp/default-text-area))

(put-in text-area-state [:gb :offset] [10 6])
(put-in text-area-state2 [:gb :offset] [10 6])

(defonce file-open-state
  (frp/default-text-area
    :binds
    (-> (table/clone i/file-open-binds)
        (merge-into
          @{:escape
            (fn [props]
              (e/put! sh-state :open false)
              (e/put! state/focus123 :focus text-area-state))

            :enter (fn [props]
                     (fh/load-file text-area-state (string ((gb/commit! props) :text)))
                     (e/put! sh-state :open false)
                     (e/put! state/focus123 :focus text-area-state))}))))


(varfn search
  [props]
  (let [search-term (string (gb/content props))
        gb (text-area-state :gb)]
    (gb/put-caret gb (if (gb :selection)
                       (max (gb :selection)
                            (gb :caret))
                       (gb :caret)))
    (print "searching :O")
    (when-let [i (gb/gb-find-forward! gb search-term)]
      (-> gb
          (gb/put-caret i)
          (put :selection (gb/gb-find-backward! gb search-term))
          (put :changed-selection true)))))

(varfn search-backward2
  [props]
  (let [search-term (string (gb/content props))
        gb (text-area-state :gb)]
    (gb/put-caret gb (if (gb :selection)
                       (min (gb :selection)
                            (gb :caret))
                       (gb :caret)))
    (print "searching bbbb :O")
    (when-let [i (gb/gb-find-backward! gb search-term)]
      (-> gb
          (gb/put-caret i)
          (put :selection (gb/gb-find-forward! gb search-term))
          (put :changed-selection true)))))

(defonce search-binds @{})

(-> search-binds
    (merge-into
      @{:escape
        (fn [props]
          (e/put! sh-state :search false)
          (e/put! state/focus123 :focus text-area-state)
          state/focus123)

        :enter search

        :control @{:f search

                   :b search-backward2}})

    (table/setproto i/global-keys))

(defonce search-state
  (frp/default-text-area :binds search-binds))

(put-in search-state [:gb :background] 0x000000)
(put-in file-open-state [:gb :background] 0x000000)


(defn open-file
  [_]
  (e/put! state/focus123 :focus frp/file-open-area))

(update text-area-state :gb
        (fn [gb]
          (-> gb
              (put :show-line-numbers true)
              (put :search
                   (fn [props]
                     (e/put! sh-state :search true)
                     (e/put! state/focus123 :focus search-state)))
              (put :open-file (fn [_]
                                (e/put! sh-state :open true)
                                (e/put! state/focus123 :focus file-open-state))))))

(put text-area-state
     :search
     (fn [props]
       (e/put! sh-state :search true)
       (e/put! state/focus123 :focus search-state)))


(varfn gb-rec
  [{:offset offset
    :position position
    :size size
    :scroll scroll
    :conf conf}]

  (def [ox oy] offset)
  (def [x-pos y-pos] position)

  (def y-offset (+ y-pos oy))
  (def x-offset (+ x-pos ox))

  [x-offset
   y-offset
   (match (size 0)
     :max (- (get-screen-height) y-offset)
     w w)
   (match (size 1)
     :max (- (get-screen-height) y-offset)
     h h)])

(defn text-area
  [props & _]
  (def {:state state
        :max-width max-width
        :max-height max-height
        :height height
        :text/color text/color
        :text/size text/size
        :text/font text/font
        :text/line-height text/line-height
        :text/spacing text/spacing
        :width width} props)

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
                (put :height (max (get-in state [:gb :conf :size])
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

                      (when (= state text-area-state)
                        #(pp new-ev)
                        #
)

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
