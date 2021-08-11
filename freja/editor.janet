(import ./textarea :as ta :fresh true)
(import ./theme :as t)
(import freja/input :as i)
(import freja/events :as e)
(import freja/state)
(import freja/file-handling :as fh)
(import freja/new_gap_buffer :as gb)

(use profiling/profile)

(varfn search
  [props]
  (p :search
     (let [search-term (string (gb/content props))
           gb (props :search-target)]
       (gb/put-caret gb (if (gb :selection)
                          (max (gb :selection)
                               (gb :caret))
                          (gb :caret)))
       (print "searching :O")
       (when-let [i (gb/gb-find-forward! gb search-term)]
         (-> gb
             (gb/put-caret i)
             (put :selection (gb/gb-find-backward! gb search-term))
             (put :changed-selection true))))))

(varfn search-backwards
  [props]
  (p :search-back
     (let [search-term (string (gb/content props))
           gb (props :search-target)]
       (gb/put-caret gb (if (gb :selection)
                          (min (gb :selection)
                               (gb :caret))
                          (gb :caret)))
       (print "searching bbbb :O")
       (when-let [i (gb/gb-find-backward! gb search-term)]
         (-> gb
             (gb/put-caret i)
             (put :selection (gb/gb-find-forward! gb search-term))
             (put :changed-selection true))))))


(def file-open-binds
  (-> @{}
      (table/setproto i/file-open-binds)
      (merge-into
        @{:escape (fn [props] (:escape props))
          :enter (fn [props] (:enter props))})))

(def search-binds
  (-> @{}
      (table/setproto i/global-keys)
      (merge-into
        @{:escape |(:escape $)
          :enter |(:search $)
          :control @{:f |(:search $)
                     :b |(:search-backwards $)}})))

(defn editor
  [props & children]
  (def {:open open
        :set-open set-open
        :state state
        :initial-path initial-path
        :id id} props)

  (assert state "Must define :state")

  (unless (state :file-open)
    (put state :file-open (ta/default-textarea-state :binds file-open-binds)))

  (unless (state :search)
    (put state :search (ta/default-textarea-state :binds search-binds)))

  (unless (state :editor)
    (put state :editor (ta/default-textarea-state))

    (when initial-path
      (fh/load-file (state :editor) initial-path)))

  (def {:file-open file-open
        :search search-state
        :editor editor-state} state)

  (when id
    (put editor-state :id id))

  (put-in editor-state [:gb :open-file]
          (fn [_]
            (set-open :file-open)
            (e/put! state/focus123 :focus file-open)))

  (put-in editor-state [:gb :search]
          (fn [_]
            (set-open :search)
            (e/put! state/focus123 :focus search-state)))

  (put-in file-open [:gb :escape]
          (fn [props]
            (set-open false)
            (e/put! state/focus123 :focus editor-state)))

  (put-in file-open [:gb :enter]
          (fn [props]
            (set-open false)
            (fh/load-file editor-state (string ((gb/commit! props) :text)))
            (e/put! state/focus123 :focus editor-state)))

  (put-in search-state [:gb :search-target] (editor-state :gb))

  (put-in search-state [:gb :escape]
          (fn [props]
            (print "ESCAPE!")
            (set-open false)
            (e/put! state/focus123 :focus editor-state)))

  (put-in search-state [:gb :search] search)
  (put-in search-state [:gb :search-backwards] search-backwards)

  [:block {}
   (when-let [c (tracev (props :open))]
     [:background {:color :purple}
      [:padding {:all 4}
       (case c
         :file-open
         [:row {}
          [:text {:size 22
                  :text "Open: "}]
          [ta/textarea {:weight 1
                        :text/size 22
                        :height 28
                        :state file-open}]]

         :search
         [:row {}
          [:text {:size 22
                  :text "Search: "}]
          [ta/textarea {:weight 1
                        :text/size 22
                        :height 14
                        :state search-state}]])]]
     #
)

   [:background {:color (t/colors :background)}
    [:padding {:left 6 :top 6}
     [ta/textarea {:text/spacing 0.5
                   :text/size 20
                   :text/font "MplusCode"
                   :text/color (t/colors :text)
                   :state editor-state
                   :show-line-numbers true}]]]

   #
])
