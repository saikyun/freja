(import ./editor :as e :fresh true)
(import freja/theme :as t)
(import freja/hiccup :as h)
(import freja/events :as e)
(import freja/state)
(import freja/frp)
(use freja/defonce)

(defonce editor-state @{})

(defn text-area-hc
  [props & _]

  (unless (props :left-state)
    (put props :left-state @{}))

  (unless (props :right-state)
    (put props :right-state @{}))

  [:padding {:left 0 :top 30}
   [:background {:color 0x9D9D9Dff}
    [:row {}
     [:block {:weight 1}
      [:background {:color (if (props :left-focus)
                             (t/comp-cols :background)
                             :blank)}
       [:padding {:all 2}
        [e/editor {:state (props :left-state)
                   :id :left
                   :focus-on-init true
                   :initial-path state/initial-file
                   :open (props :left-open)
                   :set-open |(e/put! props :left-open $)}]]]]
     #[:block {:width 2}]
     [:block {:weight 1}
      [:background {:color (if (props :right-focus)
                             (t/comp-cols :background)
                             :blank)}
       [:padding {:all 2}
        [e/editor @{:state (props :right-state)
                    :id :right
                    :open (props :right-open)
                    :set-open |(do (print "opening: " $) (e/put! props :right-open $))}]]]]

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

  (frp/subscribe!
    state/focus
    (fn [{:focus focus}]

      (if (= focus (get-in editor-state [:left-state :editor]))
        (unless (editor-state :left-focus)
          (e/put! editor-state :left-focus true))
        (when (editor-state :left-focus)
          (e/put! editor-state :left-focus false)))

      (if (= focus (get-in editor-state [:right-state :editor]))
        (unless (editor-state :right-focus)
          (e/put! editor-state :right-focus true))
        (when (editor-state :right-focus)
          (e/put! editor-state :right-focus false))))))

#
# this will only be true when running load-file inside freja
(when ((curenv) :freja/loading-file)
  (print "reiniting :)")
  (init))
