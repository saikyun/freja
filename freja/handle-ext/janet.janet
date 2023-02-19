(import ../state)
(import freja/checkpoint)
(import freja/textarea)
(import freja/theme)
(import freja/editor)
(import ../open-file)
(import ../file-handling :as fh)
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
  (put state :freja/quit (fn [{:editor editor} cb]
                           (checkpoint/save-checkpoint (get-in editor [:gb :path]) "before quitting")

                           (def path (get-in editor [:gb :path]))
                           (if (get-in editor [:gb :ever-modified])
                             (do
                               (fh/save-before-closing (editor :gb)
                                                       |(do (cb)
                                                          (open-file/close-file path))))
                             (do
                               (open-file/close-file path)
                               (cb)))))

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
