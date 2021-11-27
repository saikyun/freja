(import freja/hiccup :as h)
(import freja/textarea :as t)

(def state @{})

(defn list-files-component
  [props]
  [:row {}
   [:block {:weight 1}
    "wat"]
   [:block {:weight 1}
    [:background {:color :green}
     "aoseuhteuoasnh"
     #[t/textarea @{:text/color :white}]
]]
   [:block {:weight 1}
    "wot"]])

(h/new-layer :list-files
             list-files-component
             state)
