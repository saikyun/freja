(use jaylib)

(import spork/test)
(import ./code_api :prefix "")
(import ./../new_menu :as menu)
(import ./textfield :as t)
(import ./frp :as frp)
(import ./../backwards2 :prefix "")
(use ./highlighting)
(import ./text_rendering :prefix "")
(import ./text_rendering_ints :prefix "" :fresh true)
(import ../build/text-rendering :prefix "")
(import ./input :prefix "")
(import ./file_handling :prefix "")
(import ./dumb :prefix "")
(import ./find_row_etc :prefix "")
(import ./new_gap_buffer :prefix "")
(import ./new_gap_buffer_util :prefix "")
(import ./render_new_gap_buffer :prefix "")
(import ./theme :prefix "")
(import spork/netrepl)
(import ./font :prefix "")
(import ./state :as state)
(setdyn :pretty-format "%.40M")

(defmacro defonce
  "Define a value once."
  [name & more]
  (when (nil? (dyn name))
    ~(def ,name ,;more)))

(defmacro varonce
  "Var a value once."
  [name & more]
  (when (nil? (dyn name))
    ~(var ,name ,;more)))

(comment
  (top-env 'ta/split-words))

(var top-env (fiber/getenv (fiber/current)))
(var font nil)
(var loop-fiber nil)

(var conf nil)
(var conf2 nil)
(var conf3 nil)

(var data @{:latest-res @""
            :quit false
            :top-env top-env})

(defn select-keys
  [t ks]
  (def nt (table/new (length ks)))
  (loop [k :in ks
         :let [v (t k)]]
    (put nt k v))
  nt)

(defn dumb-set
  "Creates a table that acts like a set, where the value is equal to the key."
  [& ks]
  (table ;(flatten (map (fn [k] [k k]) ks))))

(defn remove-keys
  [t ks]
  (def nt @{})
  (loop [[k v] :pairs t
         :when (not (ks k))]
    (put nt k v))
  nt)

(varfn frame
  [dt]
  (clear-background :blank)
  #  (draw-text* (conf :text) (string (data :latest-res)) [605 660] :blue)
)

(var texture nil)

(defn styling-worker
  [parent]
  (def content (thread/receive))
  (def res (peg/match styling-grammar content))
  (:send parent [:hl res]))

(varfn internal-frame
  []
  (def dt (get-frame-time))

  (put data :dt dt)

  (def [x-scale y-scale] screen-scale)

  (def w (* x-scale (get-screen-width)))
  (def h (* y-scale (get-screen-height)))

  (begin-drawing)

  (clear-background (colors :background))


  (frp/trigger dt)

  (end-drawing)

)

(defn loop-it
  []

  (set loop-fiber
       (ev/call (fn [] (while true
                         (when (data :quit)
                           (close-window)
                           (os/exit)
                           (error "QUIT!"))

                         (try
                           (do (internal-frame)
                             (ev/sleep 0.01))
                           ([err fib]
                             (let [path "text_experiment_dump"]
                               (debug/stacktrace fib err)
                               ## TODO:  Dump-state
                               #(dump-state path gb-data)
                               (print "Dumped state to " path))
                             (print (debug/stacktrace fib err))
                             (ev/sleep 1))))))))

(defn load-font
  [text-data opts]
  (let [font (load-font-ex (opts :font-path) (opts :size) (opts :glyphs))]

    (put opts :font font)

    (def t (freeze opts))

    (put text-data :conf t)
    (put text-data :sizes (glyphs->size-struct t (t :glyphs)))

    {:text t
     :colors colors}))

(defn run-init-file
  []
  (def env (data :top-env))
  (try
    (do
      (dofile (string state/freja-dir "./init.janet")
              #             :env (fiber/getenv (fiber/current))
              :env env)
      (merge-into env env))
    ([err fib]
      (print "nope")
      (print (debug/stacktrace fib err)))))

(defn start
  []
  (try
    (do
      (set-config-flags :vsync-hint)
      (set-config-flags :window-highdpi)
      (set-config-flags :window-resizable)

      (init-window 900 700
                   "Freja")

      (set-exit-key :f12) ### doing this because I don't have KEY_NULL

      (let [[xs ys] (get-window-scale-dpi)]
        (put screen-scale 0 xs)
        (put screen-scale 1 ys))

      (let [[x-scale y-scale] screen-scale
            tc @{:font-path (string state/freja-dir "./assets/fonts/Monaco.ttf")
                 :size (* 18 x-scale)
                 :line-height 1.2
                 :mult (/ 1 x-scale)
                 :glyphs default-glyphs
                 :spacing 0.5}

            tc2 @{:font-path (string state/freja-dir "./assets/fonts/Texturina-VariableFont_opsz,wght.ttf")
                  :line-height 1.1
                  :size (* 20 x-scale)
                  :mult (/ 1 x-scale)
                  :glyphs (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~\\")
                  :spacing 2}]

        (set conf (load-font state/gb-data tc))
        (put state/gb-data :context data)
        (put state/gb-data :screen-scale [x-scale y-scale])
        (put state/gb-data :colors colors)

        (set conf (load-font state/search-data tc))
        (put state/search-data :context data)
        (put state/search-data :screen-scale [x-scale y-scale])
        (put state/search-data :colors colors)

        (set conf2 (load-font state/file-open-data tc))
        (put state/file-open-data :context data)
        (put state/file-open-data :screen-scale [x-scale y-scale])
        (put state/file-open-data :colors colors)

        (put data :mouse (new-mouse-data))

        (set-target-fps 60)

        (run-init-file)

        (menu/init)

        (set texture (load-render-texture 500 500))

        (loop-it)))
    ([err fib]
      (print "error! " err)
      (debug/stacktrace fib err)

      (let [path "text_experiment_dump"]
        ## TODO:  Dump-state
        #(dump-state path gb-data)
        (print "Dumped state to " path))

      (close-window))))

(def env (fiber/getenv (fiber/current)))

(var server nil)

(defn main [& args]
  #(set server (netrepl/server "127.0.0.1" "9365" env))
  (pp args)
  (buffer/push-string state/freja-dir (os/getenv "FREJA_PATH"))
  (buffer/push-string state/freja-dir "/")
  (when-let [file (get args 1)]
    (load-file state/gb-data file)
    )
  (start))