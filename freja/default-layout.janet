(import freja/editor :as e)
(import freja/theme :as t)
(import freja/hiccup :as h)
(import freja/events :as e)
(import freja/file-handling :as fh)
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
               :initial-file state/initial-file
               :open (props :left-open)
               :set-open |(e/put! props :left-open $)}]]])

(defn default-right-editor
  [props & _]
  (def {:bottom bottom
        :bottom-h bottom-h} props)
  [:block {:height 100
           :width 100}
   [:background {:color (if (props :right-focus)
                          (t/comp-cols :background)
                          :blank)}
    [:padding {:all 2}
     [e/editor @{:state (props :right-state)
                 :id :right
                 :open (props :right-open)
                 :set-open |(do (print "opening: " $)
                              (e/put! props :right-open $))}]]]])


(defn text-area-hc
  [props & _]

  (def {:bottom bottom
        :bottom-h bottom-h} props)

  (unless (props :left-state)
    (put props :left-state @{}))

  (unless (props :right-state)
    (put props :right-state @{}))

  [:padding {:left 0 :top 30}
   [:background {:color (t/colors :background)}
    [:column {}
     [:row {:weight 1}
      [:column {}
       #
       (when (props :left)
         [:block {:weight 1}
          [(props :left) props]])
       #
       (when (props :old-left)
         [:block {}
          [:clickable
           {:on-click
            (fn [_]
              (def [compo state] (props :old-left))

              (e/put! props :left compo)
              (e/put! props :left-state state)
              #
)}
           [:text {:color :red
                   :size 24
                   :text "huh?" # (get props :old-left "nothing")
}]]])
       #
]
      #[:block {:width 2}]

      (when (or (props :right)
                (props :bottom-right))
        [:block {:weight 1}
         [:column {}
          (when (props :right)
            (if (or (not (props :bottom-right))
                    (props :right-focus))
              [:block {:weight 1}
               [(props :right) props]]
              [(props :right) props]))

          (when (props :bottom-right)
            [(props :bottom-right) props])]])

      #
]

     (when bottom
       [:block {}
        [bottom props]])]

    #
]])

(comment
  (e/put! state/editor-state :right
          (fn [props & _]
            "hej"))

  #
)

# exposing the hiccup layer for debugging purposes
(var hiccup-layer nil)

(comment
  (use freja-layout/compile-hiccup)

  # here we can print the element tree
  # in a decently readable form
  (-> (get-in c [:root])
      print-tree)
  #
)

(comment
  (print (string/format "%P" (keys (get-in state/editor-state [:left-state :editor :gb]))))
  #
)

(defn init
  []
  (set hiccup-layer (h/new-layer
                      :text-area
                      text-area-hc
                      state/editor-state))

  (e/put! state/editor-state
          :left
          default-left-editor)

  (e/put! state/editor-state
          :right
          nil
          #default-right-editor
)

  (comment
    (get state/editor-state :old-left)
    (get state/editor-state :last-left)
    #
)

  (frp/subscribe!
    state/editor-state
    (fn [{:last-left last-left
          :old-left old-left
          :left left
          :left-state left-state}]
      (when (not= last-left [left left-state])
        (when last-left
          (e/put! state/editor-state :old-left last-left))
        (e/put! state/editor-state :last-left [left left-state]))))

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
