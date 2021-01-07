(use jaylib)
(import ./textfield :as t)
(import ./textarea :prefix "" :fresh true)
(import ./text_rendering :prefix "")
(import ./text_api :prefix "")
(import ./input :prefix "")
(import ./find_row_etc :prefix "")
(import ./highlight :prefix "")
(import spork/netrepl)

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
                 :offset 10
                 
                 :caret-pos [0 0]
                 :blink 0})

(var text-data2 @{:selected @""
                  :text @""
                  :after @""
                  :dir nil
                  :scroll 0
                  
                  :position [605 5]
                  :w 590
                  :offset 10
                  
                  :caret-pos [0 0]
                  :blink 0})

(var mouse-data (new-mouse-data))
(var conf nil)
(var conf2 nil)

(var data @{:latest-res @""
            :text-data text-data
            :quit false
            :top-env top-env})

(varfn game-frame
  [dt]
  (let [y (+ (if-let [{:y y :h h} (last (text-data :rows))]
               (+ y h)
               0)
             16 120)]
    (draw-text (conf :text) (data :latest-res) [30 y] :blue))
  
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
  []
  (handle-mouse mouse-data text-data)
  (handle-scroll text-data2)
  
  (def dt (get-frame-time))
  
  (begin-drawing)
  
  (rl-viewport 0 0 (* 2 (get-screen-width))
               (* 2 (get-screen-height)))
  
  (clear-background (colors :background))
  
  #(t/render-textfield conf text-data)
  (render-textarea conf text-data)
  
  (try
    (game-frame dt)
    ([err fib]
     (print "hmm")
     (put data :latest-res (string "Error: " err))
     (print (debug/stacktrace fib err))
     ))
  
  
  (draw-text (text-data :conf) (string (text-data :current-row)) [615 10] :white)
  
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
                                           :sizes))))
  (render-textarea conf2
                   text-data2
                   )
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
                           (do (frame)
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

(defn start
  []
  (try
    (let [tc @{:font-path "./assets/fonts/Texturina-VariableFont_opsz,wght.ttf"
               :size 80
               :glyphs (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäååæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ")
               :spacing 2}
          
          tc2 @{:font-path "./assets/fonts/Texturina-VariableFont_opsz,wght.ttf"
                :size 40
                :glyphs (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäååæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ")
                :spacing 2}]
      (set-config-flags :vsync-hint)
      (set-config-flags :window-resizable)
      (init-window 1200 700
                   "Textfield")
      
      (set conf (load-font text-data tc))
      (set conf2 (load-font text-data2 tc2))
      
      (set-target-fps 60)
      
      (loop-it))
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

