(import freja/editor :as e)
(import freja/theme :as t)
(import freja/hiccup :as h)
(import freja/events :as e)
(import freja/file-handling :as fh)
(import freja/new_gap_buffer :as gb)
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
    [e/editor {:state props
               :id :left
               :focus-on-init true
               :initial-file state/initial-file
               :open (props :open)
               :set-open |(do (print "opening: " $)
                            # TODO: remove
                            (e/put! state/editor-state :force-refresh true)
                            (e/put! props :open $))}]]])

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
                  (e/put! state/editor-state :other nil)
                  (when (state :freja/focus)
                    (:freja/focus state)))}
     [:padding {:all 4}
      [:text {:size 16
              :color :white
              :text "O"}]]]
    
    [:clickable
     {:on-click (fn [_]
                  (e/put! state/editor-state :other nil)
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
                      (e/put! state/editor-state :other hiccup))
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
                  (e/put! state/editor-state :other hiccup))}
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
                    (e/put! state/editor-state :other nil)
                    (when (state :freja/focus)
                      (:freja/focus state)))}
       [:padding {:all 4}
        [:text {:size 16
                :color :white
                :text "O"}]]])
    
    (unless cant-close
      [:clickable
       {:on-click (fn [_]
                    (state/remove-buffer-stack hiccup)
                    (when-let [[_ top-state] (last (state/editor-state :stack))]
                      (when (:freja/focus top-state)
                        (:freja/focus top-state))))}
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
  # (put state/editor-state :stack @[[default-left-editor @{}]])
  (unless (state/editor-state :stack)
    (put state/editor-state :stack @[]))

  (set hiccup-layer (h/new-layer
                      :text-area
                      text-area-hc
                      state/editor-state))

  #  (e/put! state/editor-state
  #         :stack
  #        default-left-editor)

  (e/put! state/editor-state
          :right
          nil
          #default-right-editor
          )

  (comment
    (keys (get-in state/editor-state [:old-left 1 :freja/label]))
    (get state/editor-state :last-left)
    #
    )

  (frp/subscribe!
    state/focus
    (fn [{:focus focus
          :last-focut last-focus}]
      (unless (= focus last-focus)
        (e/put! state/editor-state :force-refresh true))

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
 