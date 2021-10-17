(def freja-jaylib (require "freja-jaylib"))
(use freja-jaylib)

(def state (require "./state"))
(import ./state :as state)
(put module/cache "freja/state" state)

(def defonce (require "./defonce"))
(import ./defonce :as defonce)
(put module/cache "freja/defonce" defonce)

(import spork/test)
(def code_api (require "./code_api"))
(import ./code_api :prefix "")
(put module/cache "freja/code_api" code_api)

(def assets (require "./assets"))
(import ./assets :as assets)
(put module/cache "freja/assets" assets)

(def frp (require "./frp"))
(import ./frp :as frp)
(put module/cache "freja/frp" frp)

(def events (require "./events"))
(import ./events :as events)
(put module/cache "freja/events" events)

(def text_rendering (require "./text_rendering"))
(import ./text_rendering :as text_rendering)
(put module/cache "freja/text_rendering" text_rendering)


(def fonts (require "./fonts"))
(import ./fonts :as fonts)

(def collision (require "./collision"))
(import ./collision :as collision)
(put module/cache "freja/collision" collision)


(use ./highlighting)
(import ./text_rendering :prefix "")

(def input (require "./input"))
(import ./input)
(put module/cache "freja/input" input)

(def file-handling (require "./file-handling"))
(import ./file-handling)
(put module/cache "freja/file-handling" file-handling)


(import ./dumb :prefix "")
(import ./find_row_etc :prefix "")

(def new_gap_buffer (require "./new_gap_buffer"))
(import ./new_gap_buffer :as new_gap_buffer)
(put module/cache "freja/new_gap_buffer" new_gap_buffer)

(def render_new_gap_buffer (require "./render_new_gap_buffer"))
(import ./render_new_gap_buffer :as render_new_gap_buffer)
(put module/cache "freja/render_new_gap_buffer" render_new_gap_buffer)

(def theme (require "./theme"))
(import ./theme :as theme)
(put module/cache "freja/theme" theme)

(def checkpoint (require "./checkpoint"))
(import ./checkpoint)
(put module/cache "freja/checkpoint" checkpoint)

(import spork/netrepl)
(import spork/path)
(setdyn :pretty-format "%.40M")
(import whereami :as wai)


(def hiccup (require "./hiccup"))
(import ./hiccup :as hiccup)
(put module/cache "freja/hiccup" hiccup)

(def default-hotkeys (require "./default-hotkeys"))
(import ./default-hotkeys)
(put module/cache "freja/default-hotkeys" default-hotkeys)

(def textarea (require "./textarea"))
(import ./textarea)
(put module/cache "freja/textarea" textarea)

(def editor (require "./editor"))
(import ./editor)
(put module/cache "freja/editor" editor)

(def echoer (require "./echoer"))
(import ./echoer :as echoer)
(put module/cache "freja/echoer" echoer)

(def vector-math (require "./vector-math"))
(import ./vector-math :as vector-math)
(put module/cache "freja/vector-math" vector-math)

(def flow (require "./flow"))
(import ./flow :as flow)
(put module/cache "freja/flow" flow)

(import ./newest-menu :as menu)
(import ./default-layout)

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
  # TODO: fix with new thherad stuff
  #  (def content (thread/receive))
  (def res (peg/match styling-grammar content))
  (:send parent [:hl res]))

(use profiling/profile)

(varfn internal-frame
  []
  (def dt (get-frame-time))

  (put data :dt dt)

  (def [x-scale y-scale] screen-scale)

  (def w (* x-scale (get-screen-width)))
  (def h (* y-scale (get-screen-height)))

  (begin-drawing)

  # this seems to stop glitching when initing too big window
  (rl-load-identity)

  (clear-background :white
                    # (theme/colors :background)
)

  (frp/trigger dt)

  (end-drawing))

(defn loop-it
  []

  (set loop-fiber
       (ev/call (fn [] (while true
                         (when (or state/quit
                                   (window-should-close))
                           (when state/quit-hook
                             (state/quit-hook))

                           (checkpoint/save-file-with-checkpoint
                             (get-in state/editor-state [:left-state :editor :gb])
                             "before quitting")

                           (close-window)
                           (os/exit)
                           (error "QUIT!"))

                         (try
                           (do
                             # prints might have happened between renders
                             (unless (empty? state/out)
                               (events/push! frp/out (string state/out))
                               (buffer/clear state/out))

                             (with-dyns [:out state/out
                                         :err state/out]
                               (internal-frame)
                               (unless (empty? state/out)
                                 (events/push! frp/out (string state/out))
                                 (buffer/clear state/out))
                               (ev/sleep 0.0001)))
                           ([err fib]
                             (let [path "text_experiment_dump"]
                               (debug/stacktrace fib err)
                               ## TODO:  Dump-state
                               #(dump-state path gb-data)
                               #(print "Dumped state to " path)
)
                             (print (debug/stacktrace fib err))
                             (ev/sleep 1))))))))

(defn run-init-file
  []
  (def env (data :top-env))
  (try
    (do
      (file-handling/freja-dofile
        env
        (string state/freja-dir "init.janet")
        # :env (fiber/getenv (fiber/current))
        #:env env
))
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

      (init-window 800 600 "Freja")

      (put frp/screen-size :screen/width (get-screen-width))
      (put frp/screen-size :screen/height (get-screen-height))

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

        (put data :mouse (input/new-mouse-data))

        (set-target-fps 60)

        (run-init-file)

        (default-layout/init)
        (menu/init)
        (echoer/init)

        (set texture (load-render-texture 500 500))

        (loop-it)))
    ([err fib]
      (print "error! " err)
      (debug/stacktrace fib err)

      (let [path "text_experiment_dump"]
        ## TODO:  Dump-state
        #(dump-state path gb-data)
        (print "Dumped state to " path))

      (when state/quit-hook
        (state/quit-hook))

      (checkpoint/save-file-with-checkpoint
        (get-in state/editor-state [:left-state :editor :gb])
        "before crashing")

      (close-window))))

(def env (fiber/getenv (fiber/current)))

(var server nil)

(def version (require "./version"))
(import ./version)
(put module/cache "freja/version" version)

(set version/ver-str
     (try
       (let [proc (os/spawn ["git"
                             "rev-parse" "--short" "HEAD"]
                            :px
                            {:out :pipe})]
         (os/proc-wait proc)
         (string/trimr (ev/read (proc :out) :all)))
       ([err]
         (eprintf "failed to determine commit: %p" err)
         (os/exit 1))))


(defn main [& args]
  (when (= "--version" (get args 1))
    (print (string "freja " version/ver-str))
    (os/exit 0))

  (set file-handling/scratch-path (file-handling/data-path "scratch"))

  (when-let [syspath (os/getenv "JANET_PATH")]
    (setdyn :syspath syspath))

  (put module/cache "freja-jaylib" freja-jaylib)
  (put module/cache "freja/state" state)
  (put module/cache "freja/defonce" defonce)

  (put module/cache "freja/fonts" fonts)
  (put module/cache "freja/events" events)
  (put module/cache "freja/collision" collision)
  (put module/cache "freja/frp" frp)
  (put module/cache "freja/theme" theme)
  (put module/cache "freja/input" input)
  (put module/cache "freja/assets" assets)
  (put module/cache "freja/hiccup" hiccup)
  (put module/cache "freja/file-handling" file-handling)
  (put module/cache "freja/new_gap_buffer" new_gap_buffer)
  (put module/cache "freja/render_new_gap_buffer" render_new_gap_buffer)
  (put module/cache "freja/textarea" textarea)
  (put module/cache "freja/editor" editor)
  (put module/cache "freja/default-hotkeys" default-hotkeys)

  #(set server (netrepl/server "127.0.0.1" "9365" env))
  #(buffer/push-string derp/derp "from main")
  (pp args)

  (buffer/push-string
    state/freja-dir
    (or (os/getenv "FREJA_PATH")

        (let [path (if (= :windows (os/which))
                     (string (os/getenv "LOCALAPPDATA") path/sep "freja")
                     (string (os/getenv "HOME") path/sep ".config" path/sep "freja"))]
          (when (os/stat path)
            (string path path/sep)))

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

  (file-handling/ensure-dir (file-handling/data-path ""))

  (if-let [file (get args 1)]
    (let [[path line column]
          (peg/match
            '(* (<- (some (if-not ":" 1)))
                (opt (* ":"
                        (<- (some (if-not ":" 1)))))
                (opt (* ":"
                        (<- (some (if-not ":" 1))))))
            file)]
      (set state/initial-file [path
                               (-?> line scan-number)
                               (-?> column scan-number)]))
    (do
      (unless (os/stat file-handling/scratch-path)
        (spit file-handling/scratch-path ``
# This is your personal scratch file
# if you want to try it out, just hit Ctrl/Cmd + L

(print "Welcome to your personal scratch file!")
``))

      (set state/initial-file [file-handling/scratch-path])))

  (start)

  (print "JANET_PATH is: " (dyn :syspath)))
