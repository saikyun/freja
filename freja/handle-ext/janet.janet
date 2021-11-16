(import ../state)
(import freja/checkpoint)
(import freja/textarea)
(import freja/theme)
(import freja/editor)

(defn new-editor-state
  [{:path path}]
  (def state @{})

  (put state :editor (textarea/default-textarea-state))

  (checkpoint/load-file-with-checkpoints (state :editor) path)

  state)

(defn default-editor
  [props & _]
  (def {:bottom bottom
        :bottom-h bottom-h} props)
  [:background {:color (if (props :left-focus)
                         (theme/comp-cols :background)
                         :blank)}
   [:padding {:all 2}
    [editor/editor {:state (props :left-state)
                    :id :left
                    :focus-on-init true
                    #:initial-file state/initial-file
                    #:open (props :left-open)
                    #:set-open |(e/put! props :left-open $)
}]]])

(state/add-ext-handling
  ".janet"
  (fn [_]
    default-editor)
  new-editor-state)
