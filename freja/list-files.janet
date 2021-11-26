# I want to:
# - show a text field
# - when I write in it, search project for files matching the string

# I need to:
# - get a list of all files
# - search through those

(import spork/path)

(defn get-files
  [root]
  (try
    (let [paths (os/dir root)]
      (def res @[])
      (loop [p :in paths
             :let [p (string root path/sep p)
                   {:mode mode} (os/stat p)
                   dir? (= mode :directory)]]
        (array/push res p)
        (when dir?
          (array/concat res (get-files p))))
      res)

    ([err fib]
      (debug/stacktrace fib err))))

(defn get-files-relative
  [full-root &opt root]
  (try
    (let [paths (os/dir full-root)]
      (def res @[])
      (loop [p :in paths
             :let [fp (string full-root path/sep p)
                   {:mode mode} (os/stat fp)
                   dir? (= mode :directory)
                   relpath
                   (if-not root
                     p
                     (path/join root p))]]
        (if dir?
          (array/concat res (get-files-relative fp relpath))
          (array/push res relpath)))
      res)

    ([err fib]
      (debug/stacktrace fib err))))

(comment
  #
  (loop [v :in (get-files (os/cwd))] (print v))

  (loop [v :in (get-files-relative (os/cwd))] (print v))

  (def peg ~{:main (any (+ (* ($) "freja")
                           1))})

  (pp
    (->>
      (map (fn [p] (unless (empty? (peg/match peg p))
                     p))
           (get-files-relative (os/cwd)))
      (filter (comp not nil?))))

  #
)

(import freja/hiccup :as h)
(import freja/textarea :as t :fresh true)
(import freja/events :as e)
(import freja/open-file)
(import freja/file-handling :as fh)
(import freja/state)
(import freja/default-hotkeys :as dh)

(setdyn :pretty-format "%P")

(def state @{:files (get-files-relative (os/cwd))})

(defn list-files-component
  [props]
  (def {:search search
        :files files
        :offset offset}
    props)

  (default offset 0)

  (def peg ~{:main (any (+ (* ($) ,(or (-?> search string) 1))
                           1))})

  (def filtered-files (->> files
                           (map (fn [p] (unless (empty? (peg/match peg p)) p)))
                           (filter (comp not nil?))
                           (take 10)))
  

  (def offset (-> offset
                  (max 0)
                  (min (dec (length filtered-files)))))

  (def selected-file (tracev (get filtered-files offset)))

  [:padding {:all 0}
   [:column {}
    [:block {:weight 0.05}]
    [:row {}
     [:block {:weight 0.5}]
     [:block {:weight 1}
      [:background {:color 0x555555ff}
       [:padding {:all 6}
        [:block {} [:text {:size 24 :text "Open file"}]]
        [:padding {:top 6 :bottom 6}
         [t/textarea
          @{:text/color :white
            :init
            (fn [self _]
              (print "huh")
              (e/put! state/focus :focus (self :state)))

            :text/size 20
            :height 20
            :extra-binds
            @{:escape (fn [_]
                        (h/remove-layer :list-files state)
                        (:freja/focus (get-in state/editor-state
                                              [:stack 0 1])))
              :down (fn [_] (e/put! state :offset (inc offset)))
              :up (fn [_] (e/put! state :offset (dec offset)))
              :enter
              (fn [_]
                (open-file/open-file
                  ;(fh/string->path-line-column
                     (tracev selected-file)))
                (h/remove-layer :list-files state))}
            :on-change |(e/put! state :search $)}]]
        ;(seq [f :in filtered-files
               :let [selected (= f selected-file)]]
           (if selected
             [:background {:color 0xffffff99}
              [:text {:color 0x111111ff :size 16 :text (or selected-file "")}]]
             [:block {}
              [:padding {:top 2}
               [:text {:text f :size 16 :color :white}]]]))]]]
     [:block {:weight 0.5}]]
    [:block {:weight 1}]]])

(dh/global-set-key [:control :r]
                   (fn [_]
                     (h/new-layer :list-files
                                  list-files-component
                                  state)))
