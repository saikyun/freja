(use ./build/jaylib)
(import ./textfield :as t)
(import ./text_rendering :prefix "")
(import ./text_api :prefix "")
(import ./input :prefix "")
(import spork/netrepl)

(var top-env (fiber/getenv (fiber/current)))
(var font nil)
(var loop-fiber nil)

(def colors
  {:text       [0.396 0.478 0.514]
   :border     [0.396 0.478 0.514]
   :background [0.992 0.965 0.89]
   :selected-text [0.992 0.965 0.89]
   :selected-text-background :blue
   :caret      [0.396 0.478 0.514]})

(var text-conf nil)
(var text-data @{:selected @""
                 :text @""
                 :after @""
                 :dir nil
                 :offset 30
                 :conf text-conf})
(var mouse-data (new-mouse-data))
(var conf nil)

(var data @{:latest-res @""
            :text-data text-data
            :quit false
            :top-env top-env})

(varfn frame
  []
  (handle-keyboard data)
  (handle-mouse mouse-data text-data)
  
  (begin-drawing)
  (clear-background (colors :background))
  
  (t/render-textfield conf text-data)
  (draw-text text-conf (data :latest-res) [30 120] :blue)
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
                        (do (frame)
                            (ev/sleep 0.01))
                        ([err fib]
                         (print "loop-it err: ")
                         (print (debug/stacktrace fib err))
                         (ev/sleep 1))))))))

(defn start
  []
  (try
    (let [tc @{:font-path "./assets/fonts/Texturina-VariableFont_opsz,wght.ttf"
               :size 40
               :glyphs (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäååæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ")
               :spacing 2}]
      (init-window 800 600 "Textfield")
      (set font (load-font-ex (tc :font-path) (tc :size) (tc :glyphs)))
      (put tc :font font)
      
      (set text-conf (freeze tc))
      (set conf {:text text-conf
                 :colors colors})

      (put text-data :conf (conf :text))
      
      (set-target-fps 60)
      
      (loop-it))
    ([err fib]
     (print "error! " err)
     (debug/stacktrace fib err)
     (close-window))))

(def env (fiber/getenv (fiber/current)))

(var server nil)

(defn main [& args]
  (if (= (get args 1) "connect")
    (netrepl/client "127.0.0.1" "9365" "bob")
    (do (set server (netrepl/server "127.0.0.1" "9365" env))
        (start))))
