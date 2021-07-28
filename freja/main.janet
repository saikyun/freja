(def jaylib (require "jaylib"))
(use jaylib)

(import spork/test)
(import ./code_api :prefix "")
(import ./textfield :as t)

(def assets (require "./assets"))
(import ./assets :as assets)
(put module/cache "freja/assets" assets)

(import ./update-window-title)

(def frp (require "./frp"))
(import ./frp :as frp)
(put module/cache "freja/frp" frp)

(def events (require "./events"))
(import ./events :as events)
(put module/cache "freja/events" events)

(def state (require "./state"))
(import ./state :as state)

(def fonts (require "./fonts"))
(import ./fonts :as fonts)

(import ./collision :prefix "")
(use ./highlighting)
(import ./text_rendering :prefix "")
(import ./text_rendering_ints :prefix "" :fresh true)
(import ../build/text-rendering :prefix "")

(def input (require "./input"))
(import ./input :as input)
(put module/cache "freja/input" input)

(import ./file_handling :prefix "")
(import ./dumb :prefix "")
(import ./find_row_etc :prefix "")

(def new_gap_buffer (require "./new_gap_buffer"))
(import ./new_gap_buffer :as new_gap_buffer)
(put module/cache "freja/new_gap_buffer" new_gap_buffer)

(import ./render_new_gap_buffer :prefix "")

(def theme (require "./theme"))
(import ./theme :as theme)

(import spork/netrepl)
(import spork/path)
(setdyn :pretty-format "%.40M")
(import whereami :as wai)


(def hiccup (require "./hiccup"))
(import ./hiccup :as hiccup)
(put module/cache "freja/hiccup" hiccup)


(import ./../new_menu :as old-menu)
(import ./../newest-menu :as menu)

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

  (clear-background (theme/colors :background))

  (frp/trigger dt)

  (end-drawing))

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

(defn run-init-file
  []
  (def env (data :top-env))
  (try
    (do
      (dofile (string state/freja-dir "init.janet")
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

      (put fonts/fonts :default (fonts/default-load-font-from-memory
                                  ".otf"
                                  fonts/poppins
                                  theme/font-size))

      (set-exit-key :f12) ### doing this because I don't have KEY_NULL

      (fonts/init-default-font)
      (assets/register-default-fonts)

      (let [[xs ys] (get-window-scale-dpi)]
        (put screen-scale 0 xs)
        (put screen-scale 1 ys))

      (let [[x-scale y-scale] screen-scale
            tc @{:font-data fonts/mplus
                 :ext ".otf"
                 :size (* 20 x-scale)
                 :line-height 1.2
                 :mult (/ 1 x-scale)
                 :glyphs fonts/default-glyphs
                 :spacing 0.5}]

        (set conf (fonts/load-font-from-mem state/gb-data tc))

        (set conf (fonts/load-font-from-mem state/search-data tc))

        (set conf2 (fonts/load-font-from-mem state/file-open-data tc))

        (put data :mouse (input/new-mouse-data))

        (set-target-fps 60)

        (run-init-file)

        (old-menu/init)
        (menu/init)
        (update-window-title/init)

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
  (put module/cache "jaylib" jaylib)
  (put module/cache "freja/fonts" fonts)
  (put module/cache "freja/events" events)
  (put module/cache "freja/frp" frp)
  (put module/cache "freja/state" state)
  (put module/cache "freja/theme" theme)
  (put module/cache "freja/input" input)
  (put module/cache "freja/assets" assets)
  (put module/cache "freja/new_gap_buffer" new_gap_buffer)

  #(set server (netrepl/server "127.0.0.1" "9365" env))
  #(buffer/push-string derp/derp "from main")
  (pp args)

  (buffer/push-string
    state/freja-dir
    (or (os/getenv "FREJA_PATH")
        (when (os/stat (string (os/getenv "HOME") path/sep ".freja"))
          (string (os/getenv "HOME") path/sep ".freja" path/sep))
        (let [p (wai/get-executable-path)]
          ### unless running as script
          (unless (string/has-suffix? "janet" p)
            (path/dirname p)))
        (when-let [a (dyn :executable)]
          ### running as script
          (when (string/has-suffix? "freja/main.janet" a)
            (string (path/dirname a) ".." path/sep)))
        "./"))

  (frp/init-chans)

  (when-let [file (get args 1)]
    (load-file frp/text-area file))
  (start))
