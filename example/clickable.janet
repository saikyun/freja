(import freja/frp)
(import freja/hiccup :as h)
(import freja/events :as e)
(use freja/defonce)

(def props @{:label "Click me"})

(defn hiccup
  [props & _]
  [:padding {:left 600
             :top 30}
   [:background {:color :white}
    [:clickable
     {:on-click (fn [_]
                  (e/put! props :label
                          (string "Different label " (math/random))))}
     (props :label)]]])

(h/new-layer :pixel-editor
             hiccup
             props
             :text/size 22)
