# I want to:
# - show a text field
# - when I write in it, search project for files matching the string

# I need to:
# - get a list of all files
# - search through those

(import spork/path)
(import freja/theme)
(import freja/hiccup :as h)
(import freja/textarea :as t)
(import freja/events :as e)
(import freja/open-file)
(import freja/file-handling :as fh)
(import freja/state)

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

(defn case-insensitive-peg
  [s]
  ~(* ,;(map (fn [c]
               ~(+ ,(string/ascii-upper (string/from-bytes c))
                   ,(string/ascii-lower (string/from-bytes c)))) s)))

(comment
  #
  (peg/match (case-insensitive-peg "cat") "CAT")
  #=> @[]

  (peg/match (case-insensitive-peg "cAT") "Cat")
  #=> @[]
  #
)

(defn search-peg
  ``
  Takes a string, returns a peg finding start positions of that string.
  Matches by splitting the string by spaces, and where each space was,
  anything matches.
  
  (peg/match (search-peg "fi do") "fine dog")
  #=> @[0]
  ``
  [search]
  (let [parts (string/split " " search)]
    (var parts-peg @[])
    (loop [i :range [0 (length parts)]
           :let [p (in parts i)
                 p-peg (case-insensitive-peg p)
                 p2 (get parts (inc i))
                 p2-peg (when p2
                          (case-insensitive-peg p2))]]
      (array/push parts-peg p-peg)
      (array/push parts-peg
                  (if p2-peg
                    ~(any (if-not ,p2-peg 1))
                    ~(any 1))))
    ~{:parts (* ($) ,;parts-peg)
      :main (any (+ :parts
                    1))}))

(comment
  #
  (peg/match (search-peg "fi do") "fine dog")
  #=> @[0]
  #
)

(defn list-files-component
  [props]
  (def {:search search
        :files files
        :offset offset}
    props)

  (default offset 0)

  (def peg
    (if (or (nil? search)
            (empty? search))
      ~'(any 1)
      (search-peg search)))

  (def filtered-files (->> files
                           (map (fn [p] (unless (empty? (peg/match peg p)) p)))
                           (filter (comp not nil?))
                           (take 10)))

  (def offset (-> offset
                  (max 0)
                  (min (dec (length filtered-files)))))

  (def selected-file (get filtered-files offset))

  (defn open
    [path]
    (open-file/open-file ;(fh/string->path-line-column path))
    (h/remove-layer :list-files props))

  [:padding {:all 0}
   [:column {}
    [:block {:weight 0.05}]
    [:row {}
     [:block {:weight 0.5}]
     [:block {:weight 1}
      [:clickable {:on-click (fn [_] # only done to stop clicks from passing through
)}
       [:background {:color (theme/comp-cols :background)}
        [:padding {:all 4}
         [:block {} [:text {:size 24
                            :color (theme/comp-cols :text/color)
                            :text "Find file"}]]
         [:padding {:top 6 :bottom 6}
          [t/textarea
           @{:text/color :white
             :init
             (fn [self _]
               (e/put! state/focus :focus (self :state)))

             :text/size 20
             :height 22
             :extra-binds
             @{:escape (fn [_]
                         (h/remove-layer :list-files props)
                         (:freja/focus (in (last (state/editor-state :stack)) 1)))
               :down (fn [_] (let [new (inc offset)
                                   new (if (>= new (length filtered-files))
                                         0
                                         new)]
                               (e/put! props :offset new)))
               :up (fn [_] (let [new (dec offset)
                                 new (if (< new 0)
                                       (dec (length filtered-files))
                                       new)]
                             (e/put! props :offset new)))
               :enter (fn [_] (open selected-file))}
             :on-change |(e/put! props :search $)}]]
         [:background {:color (theme/comp-cols :bar-bg)}
          ;(seq [f :in filtered-files
                 :let [selected (= f selected-file)]]
             [:clickable {:on-click (fn [_] (open f))}
              (if selected
                [:background {:color 0xffffff99}
                 [:block {}
                  [:padding {:all 2}
                   [:text {:color 0x111111ff :size 16 :text (or selected-file "")}]]]]
                [:block {}
                 [:padding {:all 2}
                  [:text {:text f :size 16 :color :white}]]])])]]]]]
     [:block {:weight 0.5}]]
    [:block {:weight 1}]]])

(defn find-file-dialog
  [_]
  (def state @{:files (get-files-relative (os/cwd))})

  (h/new-layer :list-files
               list-files-component
               state))

(comment
  #
  (do
    (find-file-dialog nil)
    :ok)
  #
)
