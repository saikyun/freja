(import freja/frp)
(import freja/hiccup :as h)
(use freja/defonce)

(defonce props @{})

(defn hiccup
  [props & _]
  [:padding {:left 600
             :top 30}
   [:background {:color :white}
    "hello"]])

(setdyn :pretty-format "%.40M")

(h/new-layer :pixel-editor
             hiccup
             props
             :text/size 22)
