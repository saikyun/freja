(import ../state)
(import freja/checkpoint)
(import freja/textarea)
(import freja/theme)
(import freja/editor)
(import freja/events :as e)

(defn new-editor-state
  [{:path path}]
  (def state @{})

  (print "initing state for " path)

  (put state :editor (textarea/default-textarea-state))
  (put state :freja/label path)
  (put state :focus (fn [{:editor editor}]
                      (state/focus! editor)))

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
                    :id :left
                    :focus-on-init true
                    #:initial-file state/initial-file
                    :open (props :left-open)
                    :set-open |(do #TODO: REMOVE
                                 (e/put! state/editor-state :force-refresh true)
                                 (e/put! props :left-open $))}]]])

(state/add-ext-handling
  ".janet"
  (fn [_]
    default-editor)
  new-editor-state)
