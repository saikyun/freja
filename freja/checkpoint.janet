(import spork/path)
(import ./file-handling)

(varfn checkpoint-date
  []
  (let [{:year year
         :month month
         :month-day month-day} (os/date)]
    (string/format "%04d-%02d-%02d" year month month-day)))

(varfn checkpoint-time
  []
  (let [{:hours hours
         :minutes minutes
         :seconds seconds} (os/date)]
    (string/format "%02d_%02d_%02d" hours minutes seconds)))

(varfn path->checkpoint-dir
  [path]
  (let [parts (path/parts (path/abspath path))
        freja-data-dir (file-handling/data-path "")
        checkpoint-dir (string
                         freja-data-dir
                         (string/join [;(array/slice parts 0 -2)
                                       (string ".freja-checkpoint-" (last parts))] path/sep))]
    checkpoint-dir))

(varfn save-checkpoint
  [path note]
  # only allow characters that are OK in a path
  # TODO: remove more non-ok characters
  (let [note (string/replace-all path/sep "_SLASH_" note)
        note (string/replace-all ":" "_COLON_" note)
        checkpoint-dir (path->checkpoint-dir path)
        day-dir (string checkpoint-dir path/sep (checkpoint-date))
        checkpoint-path (string day-dir path/sep (checkpoint-time) " " note)]

    (reduce (fn [acc cur]
              (if-not acc
                cur
                (let [new (string acc path/sep cur)]
                  (os/mkdir new)
                  new)))
            nil
            (string/split path/sep day-dir))

    (with [f (file/open checkpoint-path :wn)]
      (with [org-f (file/open path :rn)]
        (def content (file/read org-f :all))
        (file/write f content)))

    (print "saved checkpoint: " checkpoint-path)))

(varfn list-checkpoints
  [path]
  (let [checkpoint-dir (path->checkpoint-dir path)]
    (var days-times @[])
    (loop [dir :in (os/dir checkpoint-dir)
           :let [full-dir (string checkpoint-dir path/sep dir)]]
      (array/push days-times [dir
                              (seq [file :in (os/dir full-dir)]
                                (string full-dir path/sep file))]))
    days-times))


(comment

  (use freja/state)
  (->
    (get-in editor-state [:left-state :editor :gb :path])
    list-checkpoints)
  #
)


# TODO: click to go back in time
# also make checkpoint at current point in time

(use freja/state)
(import freja/events :as e)
(import freja/theme)
(import freja/file-handling :as fh)
(import freja/render_new_gap_buffer :as rgb)

(comment
  (keys (get-in editor-state [:left-state :editor :gb]))
  #
)

(varfn format-filename
  [filename]
  (def peg
    ~{:time (/ (* ':d ':d "_")
               ,(fn [d1 d2] (string d1 d2 ":")))
      :main (* :time :time ':d ':d '(any 1))})
  (string ;(peg/match peg filename)))

(comment
  (format-filename "15_56_56 ueoh")

  #
)


(defn save-file-with-checkpoint
  [props &opt note]
  (def path (props :path))
  (print "checkpoint save: " path)

  (default note "manual save")

  (fh/save-file props)
  (save-checkpoint path note))

(varfn load-file-with-checkpoints
  [props path]
  # this happens on first opening freja, so there might not be a preexisting path
  (when (get-in props [:gb :path])
    (save-file-with-checkpoint (props :gb) (string "before opening " path)))

  (fh/load-file props path)

  # only save a checkpoint if there actually was a file here
  (when (os/stat path)
    (save-checkpoint path "after opening")))

(varfn checkpoint-list
  [props]
  (def {:path path
        :textarea textarea
        :selected selected
        :close close} props)

  [:background {:color (theme/colors :background)}
   [:padding {:all 6}
    [:block {}
     [:padding {:bottom 6}
      [:block {}
       [:clickable {:on-click (fn [_] (close))}
        "Close"]]
      [:text {:text "Checkpoints"
              :size 28}]]]

    (try
      (let [checkpoints (or (-?> path list-checkpoints) [])]
        [:block {}
         [:block {}
          [:padding {:bottom 12}
           [:text {:text (string
                           "Click on checkpoints below to restore earlier versions of:\n"
                           (path/abspath path))
                   :size 18}]]]
         ;(seq [[day times] :in (reverse (sort-by first checkpoints))]
            [:padding {:bottom 12}
             [:block {}
              [:text {:size 22
                      :text (string day)}]
              ;(seq [fullpath :in (reverse (sort times))]
                 [:clickable {:on-click
                              (fn [_]
                                (when (props :needs-save)
                                  (save-file-with-checkpoint (in textarea :gb) "before moving to checkpoint")
                                  (:put props :needs-save false))
                                (fh/load-file textarea
                                              fullpath)
                                (put-in textarea [:gb :path] path)
                                (:put props :selected fullpath)
                                (print fullpath))}
                  [:block {}
                   [:background {:color (when (= selected fullpath)
                                          (theme/colors :text))}
                    [:text {:size 18
                            :color (when (= selected fullpath)
                                     (theme/colors :background))
                            :text (format-filename (path/basename fullpath))}]]]])]])])
      ([err fib]
        (if (and (string? err)
                 (peg/find "cannot open directory" err))
          (string err "\n\nthis might be due to no checkpoints existing")
          err)))]])

(defn checkpoint-component
  [props]
  (unless (props :checkpoint-props)
    (print "new checkpoint props")
    (let [checkpoint-props
          @{:path (get-in editor-state [:left-state :editor :gb :path])
            :textarea (get-in editor-state [:left-state :editor])
            :needs-save true
            :close (fn []
                     (put props :checkpoint-props nil)
                     (e/put! editor-state :right nil))}]

      (put checkpoint-props :put
           (fn [self k v]
             (e/update! props :checkpoint-props put k v)))

      (put props :checkpoint-props checkpoint-props)))

  [:block {} [checkpoint-list (props :checkpoint-props)]])

(varfn show-checkpoints
  []
  (if-not (= (editor-state :right) checkpoint-component)
    (e/put! editor-state :right checkpoint-component)
    (do (put editor-state :checkpoint-props nil)
      (e/put! editor-state :right nil))))

#(save-checkpoint "checkpoint.janet")
#(list-checkpoints "checkpoint.janet")
#(overwrite-checkpoint "checkpoint.janet")

