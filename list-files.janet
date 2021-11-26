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
(import freja/textarea :as t)

(def state @{:files
             (get-files-relative (os/cwd))})

(defn list-files-component
  [props]
  [:background {:color :green}
   [:text {:size 30
           :color :black
           :text "hej watter 123"}]
   [t/textarea]])

(h/new-layer :list-files
             list-files-component
             state)
