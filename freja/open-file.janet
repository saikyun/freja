(import freja/state)
(import freja/events :as e)
(import freja/new_gap_buffer :as gb)
(import freja/render_new_gap_buffer :as rgb)
(import freja/theme)
(import freja/editor)
(import freja/checkpoint)
(import spork/path)

(def open-files
  @{})

(defn open-file*
  [[comp state]]
  (e/put! state/editor-state :left comp)
  (e/put! state/editor-state :left-state state))

(defn open-file
  [path &opt line column]

  (if-let [comp-state (open-files path)]
    (open-file* comp-state)
    (let [new-state (state/ext->editor (path/ext path) {:path path})]
      (put open-files path new-state)
      (open-file* new-state)))

  (let [gb (get-in open-files [path :editor :gb])]
    (when line
      (rgb/goto-line-number gb line))

    (when column
      (gb/move-n gb column))))

(comment
  (open-file "freja/main.janet")
  #
)
