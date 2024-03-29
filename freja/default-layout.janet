(import freja/editor :as ed)
(import freja/theme :as t)
(import freja/hiccup :as h)
(import freja/file-handling :as fh)
(import freja/new_gap_buffer :as gb)
(import freja/state)
(import freja/event/subscribe :as s)
(import freja/open-file)
(use freja/defonce)

(defn default-left-editor
  [props & _]
  (def {:bottom bottom
        :bottom-h bottom-h} props)
  [:background {:color (if (props :left-focus)
                         (t/comp-cols :background)
                         :blank)}
   [:padding {:all 2}
    [ed/editor {:state props
                :id :left
                :focus-on-init true
                :initial-file state/initial-file
                :open (props :open)
                :set-open |(do # TODO: remove
                             (s/put! state/editor-state :force-refresh true)
                             (s/put! props :open $))}]]])

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
     [ed/editor @{:state (props :right-state)
                  :id :right
                  :open (props :right-open)
                  :set-open |(s/put! props :right-open $)}]]]])

(defn other-row
  [{:hiccup hiccup}]
  (def [_ state] hiccup)
  [:block {}
   [:row {}
    [:clickable
     {:weight 1
      :on-click (fn [_]
                  (when (state :freja/focus)
                    (:freja/focus state)))}
     [:padding {:all 4}
      [:text {:size 16
              :color :white
              :text (state :freja/label)}]]]

    [:clickable
     {:on-click (fn [_]
                  (state/push-buffer-stack hiccup)
                  (s/put! state/editor-state :other nil)
                  (when (state :freja/focus)
                    (:freja/focus state)))}
     [:padding {:all 4}
      [:text {:size 16
              :color :white
              :text "O"}]]]

    [:clickable
     {:on-click (fn [_]
                  (s/put! state/editor-state :other nil)
                  (when-let [[_ top-state] (last (state/editor-state :stack))]
                    (when (:freja/focus top-state)
                      (:freja/focus top-state))))}
     [:padding {:all 4}
      [:text {:size 16
              :color :white
              :text "X"}]]]
    #
]])

(defn menu-column
  [{:hiccup hiccup}]
  (def [_ state] hiccup)
  [:block {}
   [:row {}
    [:clickable
     {:weight 1
      :on-click (fn [_]
                  (when (state :freja/focus)
                    (:freja/focus state)))}
     [:padding {:all 4}
      [:text {:size 16
              :color :white
              :text (state :freja/label)}]]]

    [:clickable
     {:on-click (fn [_]
                  (s/put! state/editor-state :menu-column nil)
                  (when-let [[_ top-state] (last (state/editor-state :stack))]
                    (when (:freja/focus top-state)
                      (:freja/focus top-state))))}
     [:padding {:all 4}
      [:text {:size 16
              :color :white
              :text "X"}]]]
    #
]])

(defn stack-row
  [{:hiccup hiccup
    :put-in-other put-in-other
    :cant-close cant-close}]
  (def [_ state] hiccup)

  [:block {}
   [:row {}
    [:clickable
     {:weight 1
      :on-click (fn [_]
                  (if put-in-other
                    (do
                      (when-let [o (state/editor-state :other)]
                        (state/push-buffer-stack o))
                      (state/remove-buffer-stack hiccup)
                      (s/put! state/editor-state :other hiccup))
                    (state/push-buffer-stack hiccup))
                  (when (state :freja/focus)
                    (:freja/focus state)))}
     [:padding {:all 4}
      [:text {:size 16
              :color :white
              :text (state :freja/label)}]]]

    [:clickable
     {:on-click (fn [_]
                  (when-let [o (state/editor-state :other)]
                    (state/push-buffer-stack o))
                  (state/remove-buffer-stack hiccup)
                  (s/put! state/editor-state :other hiccup))}
     [:padding {:all 4}
      [:text {:size 16
              :color :white
              :text "->"}]]]

    (when-let [o (state/editor-state :other)]
      [:clickable
       {:on-click (fn [_]
                    (state/remove-buffer-stack hiccup)
                    (state/push-buffer-stack o)
                    (state/push-buffer-stack hiccup)
                    (s/put! state/editor-state :other nil)
                    (when (state :freja/focus)
                      (:freja/focus state)))}
       [:padding {:all 4}
        [:text {:size 16
                :color :white
                :text "O"}]]])

    (unless cant-close
      [:clickable
       {:on-click (fn [_]
                    (def quit-fn? (state :freja/quit))

                    (if quit-fn?
                      (:freja/quit state
                                   (fn []

                                     (state/remove-buffer-stack hiccup)
                                     (when-let [[_ top-state] (last (state/editor-state :stack))]
                                       (when (:freja/focus top-state)
                                         (:freja/focus top-state)))))

                      (do
                        (state/remove-buffer-stack hiccup)
                        (when-let [[_ top-state] (last (state/editor-state :stack))]
                          (when (:freja/focus top-state)
                            (:freja/focus top-state))))))}
       [:padding {:all 4}
        [:text {:size 16
                :color :white
                :text "X"}]]])
    #
]])

(defn text-area-hc
  [props & _]

  (def {:bottom bottom
        :bottom-h bottom-h} props)

  (unless (props :right-state)
    (put props :right-state @{}))

  [:padding {:left 0 :top 30}
   [:background {:color (t/colors :background)}
    [:column {}
     [:row {:weight 1}
      #
      (when-let [o (props :menu-column)]
        [:column {:weight 0.3}
         [menu-column {:hiccup o}]
         [:block {:weight 1} o]])
      #
      (unless (empty? (props :stack))
        (let [s (reverse (props :stack))
              top (first s)
              rest (take 3 (drop 1 s))]
          [:column {:weight 1}
           [stack-row {:hiccup top
                       :put-in-other true
                       :cant-close (empty? rest)}]
           [:block {:weight 1}
            top]
           ;(seq [hiccup :in rest
                  :let [[compo state] hiccup]]
              [stack-row {:hiccup hiccup}])]))

      (when-let [o (props :other)]
        [:column {:weight 1}
         [other-row {:hiccup o}]
         [:block {:weight 1} o]])

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

# exposing the hiccup layer for debugging purposes
(var hiccup-layer nil)

(defn init
  []
  (unless (state/editor-state :stack)
    (put state/editor-state :stack @[]))

  (set hiccup-layer (h/new-layer
                      :text-area
                      text-area-hc
                      state/editor-state)))

(comment
  (use freja-layout/compile-hiccup)

  # here we can print the element tree
  # in a decently readable form
  (-> (get-in hiccup-layer [:root])
      print-tree)

  #
)


#
# this will only be true when running load-file inside freja
(when (dyn :freja/loading-file)
  (print "reiniting :)")
  (init))
