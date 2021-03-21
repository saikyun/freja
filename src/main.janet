(use jaylib)
(import spork/test)
(import ./code_api :prefix "")
(import ./textfield :as t)
(import ./text_rendering :prefix "")
(import ./text_rendering_ints :prefix "" :fresh true)
(import ../build/text-rendering :prefix "")
(import ./input :prefix "")
(import ./file_handling :prefix "")
(import ./find_row_etc :prefix "")
(import ./highlight :prefix "")
(import ./new_gap_buffer :prefix "")
(import ./new_gap_buffer_util :prefix "")
(import ./render_new_gap_buffer :prefix "")
(import spork/netrepl)

(setdyn :pretty-format "%.40M")

(defmacro defonce
  "Define a value once."
  [name & more]
  (when (nil? (dyn name))
    ~(def ,name ,;more)))

(comment
  (top-env 'ta/split-words))

(var top-env (fiber/getenv (fiber/current)))
(var font nil)
(var loop-fiber nil)

(def colors
  {:text [0.396 0.478 0.514]
   :border [0.396 0.478 0.514]
   :background (map |(/ $ 255) [230 227 213]) #[0.992 0.965 0.89]
   :textarea [0.992 0.965 0.89]
   :selected-text [0.992 0.965 0.89]
   :selected-text-background :blue
   :caret [0.396 0.478 0.514]

   :game-bg (map |(/ $ 255) [134 173 173])

   :special-symbol (map |(/ $ 255) [133 153 0])
   :string (map |(/ $ 255) [42 161 151])
   :keyword (map |(/ $ 255) [38 138 210])})

(varfn focus
  [{:id id :context context}]
  (set delay-left @{})
  (put context :focus id))

(varfn focus-other
  [{:context context} id]
  (set delay-left @{})
  (put context :focus id))

(var gb-data @{})

(merge-into gb-data
            @{:text @""
              :gap-start 0
              :gap-stop 0
              :gap @""
              :caret 0

              :actions @[]
              :redo-queue @[]

              :selection nil

              :size [500 800]
              :position [0 30]
              :offset [10 0]
              
              :changed true
              :scroll 0
              :blink 0
              
              :open-file (fn [props]
                           (focus-other props :open-file))
              
              :save-file (fn [props]
                           (if-let [path (props :path)]
                             (save-file props path)
                             (print "no path!")))
              
              :binds gb-binds})

(def debug-data @{})

(merge-into debug-data
            @{:text @"ok"
              :gap-start 0
              :gap-stop 0
              :gap @""
              :caret 0
              
              :actions @[]
              :redo-queue @[]
              
              :selection nil
              
              :size [500 800]
              :position [500 30]
              :offset [10 0]
              
              :changed true
              :scroll 0
              :blink 0
              
              :open-file (fn [props]
                           (focus-other props :open-file))
              
              :save-file (fn [props]
                           (if-let [path (props :path)]
                             (save-file props path)
                             (print "no path!")))
              
              :binds gb-binds})

(var file-open-data @{})

(merge-into file-open-data
            @{:text @""
              :gap-start 0
              :gap-stop 0
              :gap @""
              :caret 0
              
              :actions @[]
              :redo-queue @[]
              :selection nil
              
              :size [800 14]
              :position [0 0]
              :offset [30 0]
              
              :changed true
              :scroll 0
              :blink 0
              
              :on-enter (fn [props path]
                          (load-file gb-data path)
                          (put gb-data :path path)
                          (focus-other props :main))
              
              :binds file-open-binds})

(var mouse-data (new-mouse-data))
(var conf nil)
(var conf2 nil)
(var conf3 nil)

(var data @{:latest-res @""
            :focus :main
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
  (draw-text (conf :text) (string (data :latest-res)) [605 660] :blue))

(var texture nil)

(varfn draw-frame
  [dt]
  (begin-texture-mode texture)
  (rl-push-matrix)
  
  (try (frame dt)
    ([err fib]
      (print "hmm")
      (put data :latest-res (string "Error: " err))
      (print (debug/stacktrace fib err))))
  
  (rl-pop-matrix)
  (end-texture-mode)

  (rl-push-matrix)
  (rl-mult-matrixf-screen-scale)

  (draw-texture-pro
    (get-render-texture texture)
    [0
     0
     (* 1 500)
     (* 1 (- 500))]
    [(+ (get-in gb-data [:position 0])
        (get-in gb-data [:size 0]))  
     0
     500 500]
    [0 0]
    0
    :white)
  
  (rl-pop-matrix))

(varfn internal-frame
  []
  (handle-mouse mouse-data gb-data)
  
  (handle-scroll gb-data)
  
  (def dt (get-frame-time))
  
  (def [x-scale _ _ _ _ y-scale] (get-screen-scale))
  
  (def w (* x-scale (get-screen-width)))
  (def h (* y-scale (get-screen-height)))
  
  (def changed (or (gb-data :changed)
                   (gb-data :changed-nav)))
  
  
  #(when changed (print "pre-render"))
  (gb-pre-render gb-data)
  
  (when changed
    (->> (remove-keys gb-data
                      (dumb-set :actions
                                :conf
                                :binds
                                :context
                                :colors
                                :sizes
                                :data))
         (string/format "%.40m")
         (replace-content debug-data)))
  
  (gb-pre-render debug-data)
  
  (when (= (data :focus) :open-file)
    (gb-pre-render file-open-data))
  
  (begin-drawing)
  
  (rl-viewport 0 0 w h)
  (rl-matrix-mode :rl-projection)  
  (rl-load-identity)  
  (rl-ortho 0 w h 0 0 1) # Recalculate internal projection matrix      
  (rl-matrix-mode :rl-modelview) # Enable internal modelview matrix        
  (rl-load-identity) # Reset internal modelview matrix
  
  (clear-background (colors :background))
  
  (gb-render-text gb-data)
  (render-cursor gb-data)
  
  #(when changed (print "render"))
  (gb-render-text debug-data)
  
  #(print)
  
  (when (= (data :focus) :open-file)
    (gb-render-text file-open-data))
  
  #(draw-frame dt)
  
  (end-drawing)
  
  (try
    (if (= (data :focus) :main)
      (handle-keyboard data gb-data dt)
      (handle-keyboard data file-open-data dt))

    ([err fib]
      (print "kbd")
      (put data :latest-res (string "Error: " err))
      (print (debug/stacktrace fib err)))))

(comment
  (ez-gb file-open-data)
  (ez-gb gb-data)

  (focus {:context data :id :main}))

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
  (def f (file/open "./init.janet" :r))
  (when f
    (def res (file/read f :all))
    (file/close f)
    (try
      (do (fiber/setenv (fiber/current) (data :top-env))
        (eval-string res))
      ([err fib]
        (put data :latest-res err)))))

(defn start
  []
  (try
    (do (set-config-flags :vsync-hint)
      (set-config-flags :window-resizable)

      (init-window 1200 700
                   "Textfield")

      (let [[x-scale _ _ _ _ y-scale] (get-screen-scale) # must be run after `init-window`
            tc @{:font-path "./assets/fonts/Monaco.ttf"
                 :size (* 14 x-scale)
                 :line-height 1.2
                 :mult (/ 1 x-scale)
                 :glyphs (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~\\")
                 :spacing 0.5}

            tc2 @{:font-path "./assets/fonts/Texturina-VariableFont_opsz,wght.ttf"
                  :line-height 1.1
                  :size (* 20 x-scale)
                  :mult (/ 1 x-scale)
                  :glyphs (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~\\")
                  :spacing 2}]

        (set conf (load-font gb-data tc))
        (put gb-data :context data)
        (put gb-data :screen-scale [x-scale y-scale])
        (put gb-data :colors colors)
        
        (set conf (load-font debug-data tc))
        (put debug-data :context data)
        (put debug-data :screen-scale [x-scale y-scale])
        (put debug-data :colors colors)        

        (set conf2 (load-font file-open-data tc))
        (put file-open-data :context data)
        (put file-open-data :screen-scale [x-scale y-scale])
        (put file-open-data :colors colors)

        (set-target-fps 60)

        (run-init-file)
        
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
  (do (set server (netrepl/server "127.0.0.1" "9365" env))
    (start)))
