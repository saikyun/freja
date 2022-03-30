(import ../state)
(import freja/checkpoint)
(import freja/textarea)
(import freja/theme)
(import freja/editor)
(import freja/event/subscribe :as s)

(defn new-editor-state
  [{:path path}]
  (def state @{})

  (put state :editor (textarea/default-textarea-state))
  (put state :freja/label path)
  (put state :freja/focus (fn [{:editor editor}]
                            (state/focus! editor)))
  (put state :freja/focus? (fn [{:editor editor}]
                               (= editor (state/focus :focus))))
  (put state :freja/quit (fn [{:editor editor}]
                           (checkpoint/save-file-with-checkpoint
                             (editor :gb)
                             "before quitting")))

  (checkpoint/load-file-with-checkpoints (state :editor) path)

  state)

(defn default-editor
  [props & _]
  (def {:bottom bottom
        :bottom-h bottom-h} props)
  [:background {:color (if (= (state/focus :focus)
                              (props :editor))
                         (theme/comp-cols :background)
                         :blank)}
   [:padding {:all 2}
    [editor/editor {:state props
                    :text/size (get state/editor-state :text/size)
                    :id :left
                    :focus-on-init true
                    #:initial-file state/initial-file
                    :open (props :left-open)
                    :set-open |(do #TODO: REMOVE
                                 (s/put! state/editor-state :force-refresh true)
                                 (s/put! props :left-open $))}]]])

(state/add-ext-handling
  ".janet"
  (fn [_]
    default-editor)
  new-editor-state)
