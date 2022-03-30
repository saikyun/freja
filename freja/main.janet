(put module/loaders :source
     (fn source-loader
       [path args]
       (put module/loading path true)
       (defer (put module/loading path nil)
         (def env (get module/cache path))
         (default env (make-env))
         (if (string/find "spork/path" path)
           (put env :redef false)
           (put env :redef true))
         (dofile path :env env ;args))))

(import bounded-queue :as queue)

(import spork/path)
#(put-in module/cache [(first (module/find "spork/path")) :dynamic-defs] false)
#(import spork/path :fresh true)

(setdyn :redef true)

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

(import ./event/jaylib-to-events :as jaylib->events)
(import ./event/default-subscriptions)

(def subscribe (require "./event/subscribe"))
(import ./event/subscribe :as subscribe)
(put module/cache "freja/event/subscribe" subscribe)

(def evaling (require "./evaling"))
(import ./evaling :as evaling)
(put module/cache "freja/evaling" evaling)

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

(def rainbow (require "./rainbow"))
(import ./rainbow :as rainbow)
(put module/cache "freja/rainbow" rainbow)

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

(def open-file (require "./open-file"))
(import ./open-file)
(put module/cache "freja/open-file" open-file)

(def introspection (require "./introspection"))
(import ./introspection)
(put module/cache "freja/introspection" introspection)

(def textarea (require "./textarea"))
(import ./textarea)
(put module/cache "freja/textarea" textarea)

(def find-file (require "./find-file"))
(import ./find-file)
(put module/cache "freja/find-file" find-file)

(def default-hotkeys (require "./default-hotkeys"))
(import ./default-hotkeys)
(put module/cache "freja/default-hotkeys" default-hotkeys)

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

(import ./handle-ext/init :as ext-init)

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

  (jaylib->events/convert dt)

  (let [{:regular regular
         :finally finally}
        state/subscriptions]
    (subscribe/call-subscribers regular finally))

  (end-drawing))

(defn loop-it
  []

  (set loop-fiber
       (ev/call
         (fn []
           (while true
             (when (or state/quit
                       (window-should-close))
               (when state/quit-hook
                 (state/quit-hook))

               # if we're gonna quit, let's print what's in state/out
               # before quitting
               (when (not (empty? state/out))
                 (with-dyns [:out stdout]
                   (print state/out)))

               (var any-quit-failed false)
               (loop [[_ s] :in (filter |(not (nil? $))
                                        [;(state/editor-state :stack)
                                         (state/editor-state :other)])]
                 (try
                   (when (s :freja/quit)
                     (:freja/quit s))
                   ([err fib]
                     (set any-quit-failed true)
                     (with-dyns [:out stdout]
                       (debug/stacktrace fib err "")
                       (let [string-rep (string/format "%.40m" s)
                             dump-path (file-handling/data-path (string "dump-" (os/time) "-" (math/floor (* 100000 (math/random)))))]
                         (print "couldn't save. here's the state in text form:")
                         (print string-rep)
                         (print)
                         (print "also dumping it to: " dump-path)
                         (spit dump-path string-rep))))))

               #(checkpoint/save-file-with-checkpoint
               #  (get-in state/editor-state [:left-state :editor :gb])
               #  "before quitting")

               (if any-quit-failed
                 (set state/quit false)
                 (do
                   (close-window)
                   (os/exit)
                   (error "QUIT!"))))

             (try
               (do
                 # prints might have happened between renders
                 (unless (empty? state/out)
                   (queue/push state/out-events (string state/out))
                   (buffer/clear state/out))

                 (with-dyns [:out state/out
                             :err state/out]
                   (internal-frame)

                   # this is done so we remember that we should quit
                   # window-should-close only returns true once
                   (when (window-should-close)
                     (set state/quit true))

                   # if we're gonna quit, let's print what's in state/out
                   # before quitting
                   (when state/quit
                     (with-dyns [:out stdout]
                       (print state/out)))

                   (unless (empty? state/out)
                     (queue/push state/out-events (string state/out))
                     (buffer/clear state/out))
                   (ev/sleep 0.0001)))
               ([err fib]
                 (let [path "text_experiment_dump"]
                   (debug/stacktrace fib err "")
                   ## TODO:  Dump-state
                   #(dump-state path gb-data)
                   #(print "Dumped state to " path)
)
                 (print (debug/stacktrace fib err ""))
                 (ev/sleep 1))))))))

(defn run-file
  [path]
  #  (def env (data :top-env))
  (try
    (do
      (file-handling/freja-dofile
        top-env
        path
        # :env (fiber/getenv (fiber/current))
        #:env env
))
    ([err fib]
      (debug/stacktrace fib err ""))))

(defn run-init-file
  []
  (run-file (string state/freja-dir "init.janet")))

(defn start
  [file &keys {:no-init-file? no-init-file?}]
  (try
    (do
      (set-config-flags :vsync-hint)
      #(set-config-flags :window-highdpi)
      (set-config-flags :window-resizable)

      (init-window 800 600 "Freja")

      (put state/screen-size :screen/width (get-screen-width))
      (put state/screen-size :screen/height (get-screen-height))

      (put fonts/fonts :default (fonts/default-load-font-from-memory
                                  ".otf"
                                  fonts/poppins
                                  theme/font-size))

      (set-exit-key :f11) ### doing this because I don't have KEY_NULL

      (fonts/init-default-font)
      (assets/register-default-fonts)

      (let [[xs ys] (get-window-scale-dpi)]
        # on windows this gave 1.25, which seems to be the scale
        # to render ui elements, not really the scale of pixels
        # very confused. so rounding for now
        (put screen-scale 0 (math/round xs))
        (put screen-scale 1 (math/round ys)))

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

        (unless no-init-file?
          (run-init-file))

        (default-layout/init)
        (menu/init)
        (echoer/init)

        (if file
          (open-file/open-file file)
          (do
            (unless (os/stat file-handling/scratch-path)
              (spit file-handling/scratch-path ``
# This is your personal scratch file
# if you want to try it out, just hit Ctrl/Cmd + L

(print "Welcome to your personal scratch file!")
``))

            (open-file/open-file file-handling/scratch-path)))

        (loop-it)))
    ([err fib]
      (debug/stacktrace fib err "")

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
         #(os/exit 1)
)))

(setdyn :redef false)

(defn main
  [& args]
  (setdyn :redef true)

  (set-trace-log-level :warning)

  (put module/loaders :source
       (fn source-loader
         [path args]
         (put module/loading path true)
         (defer (put module/loading path nil)
           (def env (get module/cache path))
           (default env (make-env))
           (dofile path :env env ;args))))

  # TODO: parse args in better way
  (def no-init-file? (= (get args 2) "--no-init"))

  (when (dyn :executable)
    (fonts/init-fonts))

  (when (= "--help" (get args 1))
    (print ``
freja <file/flag>

file             path to file you want to edit

flags:
--open-init      opens your init.janet, used for customization
--paths          paths that freja uses
--version        prints freja version
``)
    (os/exit 0))

  (when (= "--version" (get args 1))
    (print (string "freja " version/ver-str))
    (os/exit 0))

  (def open-init (= "--open-init" (get args 1)))

  (set file-handling/scratch-path (file-handling/data-path "scratch"))

  (when-let [syspath (os/getenv "JANET_PATH")]
    (setdyn :syspath syspath))

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

  (when (= "--paths" (get args 1))
    (print "init.janet: " (string state/freja-dir "init.janet"))
    (print "data (e.g. checkpoints/backups): " (file-handling/data-path ""))
    (print "scratch: " file-handling/scratch-path)
    (os/exit 0))

  (put module/cache "freja-jaylib" freja-jaylib)
  (put module/cache "freja/state" state)
  (put module/cache "freja/defonce" defonce)

  (put module/cache "freja/fonts" fonts)
  (put module/cache "freja/checkpoint" checkpoint)
  (put module/cache "freja/evaling" evaling)
  (put module/cache "freja/collision" collision)
  (put module/cache "freja/event/subscribe" subscribe)
  (put module/cache "freja/theme" theme)
  (put module/cache "freja/find-file" find-file)
  (put module/cache "freja/input" input)
  (put module/cache "freja/assets" assets)
  (put module/cache "freja/hiccup" hiccup)
  (put module/cache "freja/file-handling" file-handling)
  (put module/cache "freja/open-file" open-file)
  (put module/cache "freja/introspection" introspection)
  (put module/cache "freja/new_gap_buffer" new_gap_buffer)
  (put module/cache "freja/render_new_gap_buffer" render_new_gap_buffer)
  (put module/cache "freja/textarea" textarea)
  (put module/cache "freja/editor" editor)
  (put module/cache "freja/default-hotkeys" default-hotkeys)
  (put module/cache "freja/flow" flow)
  (put module/cache "freja/rainbow" rainbow)
  (put module/cache "freja/echoer" echoer)
  (put module/cache "freja/vector-math" vector-math)

  #(set server (netrepl/server "127.0.0.1" "9365" env))
  #(buffer/push-string derp/derp "from main")

  (default-subscriptions/init)

  (file-handling/ensure-dir (file-handling/data-path ""))

  (when (= "--dofile" (get args 1))
    (if-let [path (get args 2)]
      (do
        (defn initial-dofile [_]

          (try
            (do
              (run-file path))
            ([err fib]
              (debug/stacktrace fib err "")))

          (subscribe/unsubscribe-finally! state/frame-events initial-dofile)
          #(set state/quit true)
)

        (subscribe/subscribe-finally! state/frame-events initial-dofile))

      (do (print "--dofile needs a filepath as a second argument")
        (os/exit 0))))

  (start (if open-init
           (string state/freja-dir "init.janet")
           (when-let [file (if (= "--dofile" (get args 1))
                             (get args 2)
                             (get args 1))]
             file))
         :no-init-file? no-init-file?))


(setdyn :redef true)
