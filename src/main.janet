(use jaylib)
(import ./textfield :as t)
(import ./textarea :prefix "" :fresh true)
(import ./text_rendering :prefix "")
(import ./text_api :prefix "")
(import ./input :prefix "")
(import ./file_handling :prefix "")
(import ./find_row_etc :prefix "")
(import ./highlight :prefix "")
(import spork/netrepl)

(defmacro defonce
  "Define a value once."
  [name & more]
  (when (nil? (dyn name))
    ~(def ,name ,;more)))

(comment
 (top-env 'ta/split-words)
 )

(var top-env (fiber/getenv (fiber/current)))
(var font nil)
(var loop-fiber nil)

(def colors
  {:text       [0.396 0.478 0.514]
   :border     [0.396 0.478 0.514]
   :background  (map |(/ $ 255) [230 227 213])  #[0.992 0.965 0.89]
   :textarea   [0.992 0.965 0.89]
   :selected-text [0.992 0.965 0.89]
   :selected-text-background :blue
     :caret      [0.396 0.478 0.514]
   
   :game-bg (map |(/ $ 255) [134 173 173])
   
   :special-symbol     (map |(/ $ 255) [133 153 0])
   :string     (map |(/ $ 255) [42 161 151])
   :keyword    (map |(/ $ 255) [38 138 210])
  })

(var text-data @{:selected @""
                 :text @""
                 :after @""
                 :dir nil
                 :scroll 0
                 
                 :position [5 5]
                 :w 590
                 :offset [10 10]
                 
                 :caret-pos [0 0]
                 :blink 0})

(var text-data2 @{:selected @""
                  :text @""
                  :after @""
                  :dir nil
                  :scroll 0
                  
                  :position [605 5]
                  :w 590
                  :offset [10 10]
                  
                  :caret-pos [0 0]
                  :blink 0})

(var mouse-data (new-mouse-data))
(var conf nil)
(var conf2 nil)

(var data @{:latest-res @""
            :text-data text-data
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

(varfn debug-view
  [data]
  (put text-data2 :text (string/format "%.5m" #(data :latest-res)
                                       
                                       #text-data
                                       data
                                       ))
  
  (comment  (put text-data2 :text
               (string/format "%.5m" (remove-keys text-data
                                                  (dumb-set
                                                   :text
                                                     :after
                                                     
                                                     :full-text
                                                     :styles
                                                     :positions
                                                     :conf
                                                     :data
                                                     :sizes)))))
  
  (render-textarea conf2 text-data2))

(varfn frame
  [dt]
  (let [y (+ (if-let [{:y y :h h} (last (text-data :rows))]
               (+ y h)
               0)
             16 120)]
    (draw-text (conf :text) (string (data :latest-res)) [30 y] :blue)
    )
  
  
  (comment
   (put text-data2 :text
      (string/format "%.5m" (remove-keys text-data
                                         (dumb-set
                                          :text
                                            :after
                                            
                                            :full-text
                                            :styles
                                            :positions
                                            :conf
                                            :data
                                            :sizes)))))  
  (comment
   (let [x (+ 600 5)
         y 5
         w 590
         h (- (get-screen-height) y 5)
         roundness 0.015
         segments 9
         diff 2]
     
     (draw-rectangle-rounded [x y w h] roundness segments (colors :border))
     (draw-rectangle-rounded [(+ x diff)
                              (+ y diff)
                              (- w (* 2 diff))
                              (- h (* 2 diff))]
                             roundness
                             segments
                             (colors :game-bg))))
  
  
  
  )

(varfn frame
  [dt]
  
  (draw-text (conf :text) (string (data :latest-res)) [605 660] :blue)
  )

(varfn frame
  [dt]
  (draw-text (conf :text) (string (data :latest-res)) [605 660] :blue)

  (comment (debug-view (remove-keys text-data
                                    (dumb-set
                                     :text
                                       :after
                                       
                                       :full-text
                                       :styles
                                       :positions
                                       :conf
                                       :data
                                       :sizes))))
  )

(varfn internal-frame
  []
  (handle-mouse mouse-data text-data)
  (handle-scroll text-data)
  (handle-scroll text-data2)
  
  (def dt (get-frame-time))
  
  (def [x-scale _ _ _ _ y-scale] (get-screen-scale))
  
  
  (def w (* x-scale (get-screen-width)))  
  (def h (* y-scale (get-screen-height)))  
  
  (rl-viewport 0 0 w h)  
  
  (rl-matrix-mode :rl-projection)  
  (rl-load-identity)  
  (rl-ortho 0 w h 0 0 1)         # Recalculate internal projection matrix  
  (rl-matrix-mode :rl-modelview) # Enable internal modelview matrix  
  (rl-load-identity)             # Reset internal modelview matrix  
  
  (begin-drawing)
  
  (comment
   (let [[x-scale _ _ _ _ y-scale] (get-screen-scale)] # returns a matrix with a bunch of zeroes
     (rl-viewport 0 0 (* x-scale (get-screen-width))
                  (* y-scale (get-screen-height)))))
  
  
  (clear-background (colors :background))
  
  #(t/render-textfield conf text-data)
  (render-textarea conf text-data)
  
  (try
    (frame dt)
    ([err fib]
     (print "hmm")
     (put data :latest-res (string "Error: " err))
     (print (debug/stacktrace fib err))
     ))
  
  
  #(draw-text (text-data :conf) (string (text-data :current-row)) [615 10] :white)
  
  (comment
   (draw-text 
    conf
    (string/format "%m" (remove-keys text-data
                                     (dumb-set :styles
                                        :positions
                                        :conf
                                        :data
                                        :sizes)))
    [615 40]
    :white))
  
  #(pp (keys text-data))
  
  (end-drawing)
  
  (handle-keyboard data dt)
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
                              (print "Error!")
                              (dump-state path text-data)
                              (print "Dumped state to " path))
                            (print (debug/stacktrace fib err))
                            (ev/sleep 1))))))))

(defn load-font
  [text-data opts]
  (let [font (load-font-ex (opts :font-path) (opts :size) (opts :glyphs))]
    
    (put opts :font font)
    
    (def t (freeze opts))
    
    (put text-data :conf t)
    
    {:text   t
     :colors colors}))

(comment
 (let [opts @{:font-path "./assets/fonts/Texturina-VariableFont_opsz,wght.ttf"
              :size 52
              :glyphs (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäååæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ")
              :spacing 2}]
   (set conf (load-font text-data opts)))
 )

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
                   :glyphs (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäååæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ")
                   :spacing 0.5}
              
              tc2 @{:font-path "./assets/fonts/Texturina-VariableFont_opsz,wght.ttf"
                    :line-height 1.1
                    :size (* 20 x-scale)
                    :mult (/ 1 x-scale)
                    :glyphs (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäååæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ")
                    :spacing 2}]
          (set conf (load-font text-data tc))
          (set conf2 (load-font text-data2 tc2))
          
          (set-target-fps 60)
          
          (run-init-file)
          
          (loop-it)))
    ([err fib]
     (print "error! " err)
     (debug/stacktrace fib err)
     
     (let [path "text_experiment_dump"]
       (dump-state path text-data)
       (print "Dumped state to " path))
     
     (close-window))))

(def env (fiber/getenv (fiber/current)))

(var server nil)

(defn main [& args]
  (if (= (get args 1) "connect")
    (netrepl/client "127.0.0.1" "9365" "bob")
    (do (set server (netrepl/server "127.0.0.1" "9365" env))
        (start))))

