(import freja/frp)
(import ./freja/render_new_gap_buffer :as r)
(use freja/defonce)
(use freja/theme)

(defonce thing @{})

(use freja-jaylib)

(defn draw
  [_ _]
  (comment
    (r/gb-draw-text
      @{:text/font "MplusCode"
        :text/size 20
        :text/spacing 0.5}
      "that's crazy dude. whaaaaaat\n
whtaaaaaaaaaaaaaaaaaaaaaaat" [20 30]
      #:white
      (rgba 71 93 101)))

  (r/gb-draw-text
    @{:text/font "MplusCode"
      :text/size 20
      :text/spacing 0.5}
    "that's crazy dude. whaaaaaat\n
whtaaaaaaaaaaaaaaaaaaaaaaat" [20 30]
    :white
    #(rgba 71 93 101)
)
  (print)
  (print))

(put thing :on-event draw)

(frp/subscribe-finally! frp/frame-chan thing)
(frp/subscribe-finally! frp/frame-chan pp)
