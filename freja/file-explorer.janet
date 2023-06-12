(import ./state)
(import ./event/subscribe :as s)
(import ./open-file)

(defn split
  [f arr]
  (def arr1 @[])
  (def arr2 @[])

  (loop [v :in arr]
    (if (f v)
      (array/push arr1 v)
      (array/push arr2 v)))

  [arr1 arr2])

(defn file-explorer
  [props]
  [:block {}
   [:padding {:all 6}
    ;(let [paths (os/dir (props :path))
          [dirs files] (->> (split |(= :directory ((or (os/stat (string (props :path) "/" $)) (errorf "path %p not found" $)) :mode)) paths)
                            (map sort))]
      [;(seq [path :in dirs
               :let [expanded? (get-in props [:expanded (string (props :path) "/" path)])]]
           [:block {}
            [:clickable
             {:on-click
              (fn [_]
                (s/put! state/editor-state :force-refresh true)
                (s/update! props :expanded |(update (or $ @{}) (string (props :path) "/" path) not)))}
             [:text {:color :gray
                     :size 16
                     :text path}]]

            (when expanded?
              (file-explorer @{:expanded (props :expanded)
                               :path (string (props :path) "/" path)})
              #"hello"
)])
        ;(seq [path :in files]
           [:block {}
            [:clickable
             {:on-click
              (fn [_] (open-file/open-file (string (props :path) "/" path)))}
             [:text {:color :white
                     :size 16
                     :text path}]]])])]])

(defn toggle
  [& _]
  (if (state/editor-state :menu-column)
    (s/put! state/editor-state :menu-column nil)
    (s/put! state/editor-state
         :menu-column
         [file-explorer
          @{:freja/label "File Explorer"
            :path "."}])))