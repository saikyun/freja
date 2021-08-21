(import freja/editor :as e)
(import freja/theme :as t)
(import freja/hiccup :as h)
(import freja/events :as e)
(import freja/state)
(import freja/frp)
(use freja/defonce)

(defn default-left-editor
  [props & _]
  (def {:bottom bottom
        :bottom-h bottom-h} props)
  [:background {:color (if (props :left-focus)
                         (t/comp-cols :background)
                         :blank)}
   [:padding {:all 2}
    [e/editor {:state (props :left-state)
               :id :left
               :focus-on-init true
               :initial-path state/initial-file
               # TODO: remove when :vertical is added
               :space-in-bottom (when bottom
                                  bottom-h)
               :open (props :left-open)
               :set-open |(e/put! props :left-open $)}]]])

(defn default-right-editor
  [props & _]
  (def {:bottom bottom
        :bottom-h bottom-h} props)
  [:background {:color (if (props :right-focus)
                         (t/comp-cols :background)
                         :blank)}
   [:padding {:all 2}
    [e/editor @{:state (props :right-state)
                :id :right
                :open (props :right-open)
                # TODO: remove when :vertical is added
                :space-in-bottom (when bottom
                                   bottom-h)
                :set-open |(do (print "opening: " $)
                             (e/put! props :right-open $))}]]])


(defn text-area-hc
  [props & _]

  (def {:bottom bottom
        :bottom-h bottom-h} props)

  (unless (props :left-state)
    (put props :left-state @{}))

  (unless (props :right-state)
    (put props :right-state @{}))

  [:padding {:left 0 :top 30}

   [:background {:color 0x9D9D9Dff}
    [:row {}
     [:block {:weight 1}
      [(props :left) props]]
     #[:block {:width 2}]
     [:block {:weight 1}
      [(props :right) props]]

     #
]

    (when bottom
      [:block {:height (props :bottom-h)}
       [bottom props]])

    #
]])

(comment
  (e/put! state/editor-state :right
          (fn [props & _]
            "hej"))

  #
)

(defn init
  []
  (def c (h/new-layer
           :text-area
           text-area-hc
           state/editor-state))

  (e/put! state/editor-state
          :left
          default-left-editor)

  (e/put! state/editor-state
          :right
          default-right-editor)

  (frp/subscribe!
    state/focus
    (fn [{:focus focus}]

      (if (= focus (get-in state/editor-state [:left-state :editor]))
        (unless (state/editor-state :left-focus)
          (e/put! state/editor-state :left-focus true))
        (when (state/editor-state :left-focus)
          (e/put! state/editor-state :left-focus false)))

      (if (= focus (get-in state/editor-state [:right-state :editor]))
        (unless (state/editor-state :right-focus)
          (e/put! state/editor-state :right-focus true))
        (when (state/editor-state :right-focus)
          (e/put! state/editor-state :right-focus false))))))

#
# this will only be true when running load-file inside freja
(when ((curenv) :freja/loading-file)
  (print "reiniting :)")
  (init))
