(import ./freja/defonce :prefix "")
(import freja/frp)
(import freja/state)

(defonce ta (frp/default-text-area))

(merge-into (ta :gb)
            @{:position [500 30]
              :offset [0 5]
              :background 0xffffeeff
              :size [:max 1000]})

(frp/subscribe! frp/mouse ta)
(frp/subscribe-finally! frp/frame-chan (fn [_] (:draw ta)))
