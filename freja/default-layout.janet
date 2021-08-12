(use ./init-text-areas)
(import ./editor :as e :fresh true)
(import freja/theme :as t)
(import freja/hiccup :as h)
(import freja/events :as e)
(import freja/state)
(use freja/defonce)

(defonce editor-state @{})

(defn text-area-hc
  [props & _]

  (unless (props :left-state)
    (put props :left-state @{}))

  (unless (props :right-state)
    (put props :right-state @{}))

  [:background {:color (t/comp-cols :background)}
   [:padding {:left 0 :top 30}
    [:row {}
     [:block {:weight 1}
      [e/editor {:state (props :left-state)
                 :id :left
                 :initial-path "measure-stuff.janet"
                 :open (props :left-open)
                 :set-open |(e/put! props :left-open $)}]]
     [:block {:width 2}]
     [:block {:weight 1}
      [e/editor @{:state (props :right-state)
                  :id :right
                 :initial-path "freja/render_new_gap_buffer.janet"
                  :open (props :right-open)
                  :set-open |(e/put! props :right-open $)}]]

     #
]]])

(comment
  # old way
  (defn text-area-hc
    [props & _]

    [:background {:color (t/comp-cols :background)}
     [:padding {:left 0 :top 30}
      [:row {}
       [:block {:weight 1}
        [text-area {:state text-area-state}]]]]
     #
])
  #
)

(defn init
  []
  (def c (h/new-layer
           :text-area
           text-area-hc
           editor-state))

  #(e/put! state/focus123 :focus )
)

#
# this will only be true when running load-file inside freja
(when ((curenv) :freja/loading-file)
  (print "reiniting :)")
  (init))
