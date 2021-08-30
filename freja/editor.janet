(import ./textarea :as ta :fresh true)
(import ./theme :as t)
(import ./default-hotkeys :as dh)
(import freja/events :as e)
(import freja/state)
(import freja/file-handling :as fh)
(import freja/new_gap_buffer :as gb)
(import freja/render_new_gap_buffer :as rgb)

(use profiling/profile)

(varfn search
  [props]
  (let [search-term (string (gb/content props))
        gb (props :search-target)]
    (gb/put-caret gb (if (gb :selection)
                       (max (gb :selection)
                            (gb :caret))
                       (gb :caret)))
    (when-let [i (gb/gb-find-forward! gb search-term)]
      (-> gb
          (gb/put-caret i)
          (put :selection (gb/gb-find-backward! gb search-term))
          (put :changed-selection true)))))

(varfn search-backwards
  [props]
  (let [search-term (string (gb/content props))
        gb (props :search-target)]
    (gb/put-caret gb (if (gb :selection)
                       (min (gb :selection)
                            (gb :caret))
                       (gb :caret)))
    (when-let [i (gb/gb-find-backward! gb search-term)]
      (-> gb
          (gb/put-caret i)
          (put :selection (gb/gb-find-forward! gb search-term))
          (put :changed-selection true)))))


(def file-open-binds
  (-> @{}
      (table/setproto dh/file-open-binds)
      (merge-into
        @{:escape (fn [props] (:escape props))
          :enter (fn [props] (:enter props))})))

(def search-binds
  (-> @{}
      (table/setproto dh/search-binds)))

(defn editor
  [props & children]
  (def {:open open
        :set-open set-open
        :state state
        :initial-path initial-path
        :initial-file initial-file
        :id id
        :focus-on-init focus-on-init
        # TODO: remove when :vertical is added
        :space-in-bottom space-in-bottom} props)

  (assert state "Must define :state")

  (unless (state :file-open)
    (put state :file-open (ta/default-textarea-state :binds file-open-binds)))

  (unless (state :search)
    (put state :search (ta/default-textarea-state :binds search-binds)))

  (var editor-new? false)

  (unless (state :editor)
    (print "new editor state for " id)
    (put state :editor (ta/default-textarea-state))

    (when initial-path
      (fh/load-file (state :editor) initial-path))

    (set editor-new? true))

  (def {:file-open file-open
        :search search-state
        :editor editor-state} state)

  (when id
    (put editor-state :id id))

  (put-in editor-state [:gb :open-file]
          (fn [_]
            (set-open :file-open)
            (e/put! state/focus :focus file-open)))

  (put-in editor-state [:gb :search]
          (fn [_]
            (set-open :search)
            (e/put! state/focus :focus search-state)))

  (put-in editor-state [:gb :goto-line]
          (fn [_]
            (set-open :goto-line)))

  (put-in file-open [:gb :escape]
          (fn [props]
            (set-open false)
            (e/put! state/focus :focus editor-state)))

  (put-in file-open [:gb :enter]
          (fn [props]
            (set-open false)
            ((file-open-binds :load-file) editor-state (string ((gb/commit! props) :text)))
            (e/put! state/focus :focus editor-state)))

  (put-in search-state [:gb :search-target] (editor-state :gb))

  (put-in search-state [:gb :escape]
          (fn [props]
            (set-open false)
            (e/put! state/focus :focus editor-state)))

  (put-in search-state [:gb :search] search)
  (put-in search-state [:gb :search-backwards] search-backwards)

  [:block {}
   (when-let [c (props :open)]
     [:background {:color (t/comp-cols :background)}
      [:padding {:all 4}
       (case c
         :goto-line
         [:row {}
          [:text {:size 22
                  :color (t/comp-cols :text/color)
                  :text "Go to line: "}]
          [ta/textarea {:weight 1
                        :text/size 22
                        :height 28

                        :init (defn focus-textarea-on-init [self _]
                                (e/put! state/focus :focus (self :state)))

                        :extra-binds

                        @{:escape (fn [props]
                                    (set-open false)
                                    (e/put! state/focus :focus editor-state))
                          :enter (fn [props]
                                   (set-open false)
                                   (e/put! state/focus :focus editor-state)
                                   (rgb/goto-line-number (editor-state :gb)
                                                         (scan-number (gb/content props))))}}]]

         :file-open
         [:row {}
          [:text {:size 22
                  :color (t/comp-cols :text/color)
                  :text "Open: "}]
          [ta/textarea {:weight 1
                        :text/size 22
                        :height 28
                        :state file-open}]]

         :search
         [:row {}
          [:text {:size 22
                  :color (t/comp-cols :text/color)
                  :text "Search: "}]
          [ta/textarea {:weight 1
                        :text/size 22
                        :height 28
                        :state search-state}]])]]
     #
)

   [:background {:color (t/colors :background)}
    [:padding {:left 6 :top 6}
     [ta/textarea {:init
                   (fn [self _]
                     (when editor-new?
                       (def gb (get-in state [:editor :gb]))
                       (when-let [[path line column] initial-file]
                         (fh/load-file (state :editor) path)
                         (when line
                           (rgb/goto-line-number gb line))
                         (when column
                           (gb/move-n gb column))))

                     (when focus-on-init
                       (e/put! state/focus :focus editor-state)))
                   :text/spacing 0.5
                   :text/size 20
                   :text/font "MplusCode"
                   :text/color (t/colors :text)
                   :state editor-state
                   :show-line-numbers true
                   :space-in-bottom space-in-bottom}]]]

   #
])
