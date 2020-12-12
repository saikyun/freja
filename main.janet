# run `janet main.janet`
# in another terminal, run `janet main.janet connect`
# then modify and eval the `(varfn frame ...)` form

(use ./build/jaylib)
(import ./textfield :as t)
(import ./text_rendering :prefix "")
(import ./text_api :prefix "")
(import ./input :prefix "")
(import spork/netrepl)

(var top-env (fiber/getenv (fiber/current)))
(var quit false)
(var font nil)
(var loop-fiber nil)
#(var text @"")
#(var selected @"")
#(var selected-left-right :left)
#(var text-after @"")

(var latest-res @"aoe")

(set latest-res @"hmm")

(def colors
  {:text       [0.396 0.478 0.514]
   :border     [0.396 0.478 0.514]
   :background [0.992 0.965 0.89]
   :selected-text [0.992 0.965 0.89]
   :selected-text-background :blue
   :caret      [0.396 0.478 0.514]})

(varfn poses-in-text
  [text font-size spacing]
  (var total-w 0)
  (seq [i :range [0 (length text)]
        :let [s2 (string/slice text i (inc i)) 
              [w2 h] (measure-text-ex font s2 font-size spacing)
              w3 (math/ceil (/ w2 2))]]
    (set total-w (+ total-w w2))
    
    (def pos total-w)
    
    (if (not= i (dec (length text)))
      (set total-w (+ total-w spacing)))
    
    [i pos]))

(varfn poses-in-text
  [text font-size spacing]
  (var total-w 0)
  (seq [i :range [0 (length text)]
        :let [s2 (string/slice text i (inc i)) 
              [w2 h] (measure-text-ex font s2 font-size spacing)
              w3 (math/ceil (/ w2 2))]]
    (set total-w (+ total-w w2))
    
    (def pos total-w)
    
    (if (not= i (dec (length text)))
      (set total-w (+ total-w spacing)))
    
    [i pos]))

(varfn break-up-char
  [text spacing max-width]
  (var rows @[@[]])
  (var acc-w 0)
  (var minus 0)
  (each p text
        (def [i w] p)
        (+= acc-w (- w minus))
        (array/push (last rows) [i (- w minus)])
        (when (> acc-w max-width)
          (array/push rows @[])
          (set acc-w 0)
          (set minus (+ (get p 1) spacing))))
  rows)

(defn split-words
  [t]
  (peg/match '(any (+ (capture (some (if-not :s 1)))
                     (capture (* (if-not "\n" :s)))
                     (capture "\n"))) t))

(varfn break-up-words
  [text font-size spacing max-width]
  (var rows @[@[]])
  (var acc-w 0)
  (var minus 0)
  (def split (split-words text))
  (def widths (map |(measure-text-ex font $ font-size spacing) split))
  (loop [i :range [0 (length split)]
         :let [s (get split i)
               w (- (get (get widths i) 0) minus)]]
    (+= acc-w w)
    (+= acc-w spacing)
    (when (> acc-w max-width)
      (array/push rows @[])
      (set acc-w 0)
# (set minus (+ (get p 1) spacing))
      (+= acc-w w)
      (+= acc-w spacing))
    (array/push (last rows) [s w]))
  rows)

(comment
  (reduce (fn [acc c]
            
            (print c)) 0 text)
  
  (break-up-words "a . b" 40 2 100)
  (break-up-words text 40 2 100)
  
  (-> (poses-in-text "aoeu aoeu" 40 2)
    (break-up 100))
  
  (-> (poses-in-text "aoeu eoatnheoasnhaoeu" 40 2)
    (break-up 100))
  
  (-> (poses-in-text text 40 2)
    (break-up 2 100)
    length
    )
  
  )

(var text-conf nil)
(var text-data @{:selected @""
                 :text @""
                 :after @""
                 :dir nil
                 :offset 30
                 :conf text-conf})
(var mouse-data (new-mouse-data))
(var conf nil)

(varfn render-textarea
  [tc]
  (def {:spacing spacing
        :size font-size} tc)

  (def {:selected selected :text text :after after} text-data)
  
  (def rows (break-up-words text 40 2 280))  
  (let [x 10
        y 35
        w 300
        h (* (length rows) 40)
        roundness 0.3
        segments 9
        diff 2]
    (draw-rectangle-rounded [x y w h] roundness segments (colors :border))
    (draw-rectangle-rounded [(+ x diff)
                             (+ y diff)
                             (- w (* 2 diff))
                             (- h (* 2 diff))]
      roundness
      segments (colors :background)))  
  
  
  (let [[w h] (measure-text tc text)
        [w2 h2] (measure-text tc selected)]
    (var x-ye 0)
    (var y-ye 0)
    
    (let [_ comment]
      (var nof 0)
      
      (var y 0)
#        (print "begin")
      (loop [i :range [0 (length rows)]
             :let [l (+ ;(map (comp length first) (get rows i)))
                   w (+ ;(map |(get $ 1) (get rows i))
                       (- (* spacing (length (get rows i))) spacing))]]
#            (pp rows)
#            (print " " i " " l " " (string/slice text nof l))
        (draw-text-ex font (string/slice text nof (+ nof l)) [30 (+ 30 (* 40 i))] font-size spacing (colors :text))
        (+= nof l)
        (set x-ye 0)
        (set y-ye (* 40 i))
        (when-let [v (get (last (get rows i)) 1)]
          (set x-ye w))))
    
#        (draw-text-ex font text [30 30] font-size spacing (colors :text))
    
    (draw-rectangle-rounded [(+ 30 spacing w)
                             40
                             (+ spacing w2)
                             (- h2 10)]
      0.3
      9
      (colors :selected-text-background))
    
    (draw-text-ex font selected
      [(+ 30 spacing w) 30]
      font-size spacing (colors :selected-text))
    
    (draw-text-ex font
      (string/reverse after)
      [(+ 30 spacing w spacing w2) 30]
      font-size spacing (colors :text))
    
    (draw-line-ex [(+ 30 spacing #w
                     x-ye
                     ) #font-size
                   (+ font-size y-ye)
                   ] [(+ 30 spacing #w
                        x-ye
                        )
                      (+ #font-size
                        (+ font-size y-ye)
                        (* h 0.75))
                      
                      ] 1 (colors :caret))
    
    (draw-text-ex font latest-res [30 (+ y-ye 120)] font-size spacing :blue)))



(varfn frame
  []
  (var k (get-key-pressed))
  
  (while (not= 0 k)
    (insert-char text-data k)
    (set k (get-key-pressed)))
  
  (when (and (key-pressed? :q)
          (or (key-down? :left-control)
            (key-down? :right-control)))
    (set quit true))
  
  (when (key-pressed? :home)
    (if (or (key-down? :left-shift)
          (key-down? :right-shift))
      (select-until-beginning text-data)
      (move-to-beginning text-data)))
  
  (when (key-pressed? :end)
    (if (or (key-down? :left-shift)
          (key-down? :right-shift))
      (select-until-end text-data)
      (move-to-end text-data)))
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :.))
    (paste text-data))
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :a))
    (select-all text-data))
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :i))
    (copy text-data))
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :b))
    (cut text-data))
  
  (when (key-pressed? :backspace)
    (cond (or (key-down? :left-alt)
            (key-down? :right-alt))
          (delete-word-before text-data)
          
          (backspace text-data)))
  
  (when (key-pressed? :delete)
    (cond (or (key-down? :left-alt)
            (key-down? :right-alt))
          (delete-word-after text-data)
          
          (forward-delete text-data)))
  
  (when (key-pressed? :left)
    (cond
      ## select whole words
      (and (or (key-down? :left-alt)
             (key-down? :right-alt))
        (or (key-down? :left-shift)
          (key-down? :right-shift)))
      (select-word-before text-data)
      
      (or (key-down? :left-alt)
        (key-down? :right-alt)) 
      (move-word-before text-data)
      
      (or (key-down? :left-shift)
        (key-down? :right-shift))
      (select-char-before text-data)
      
      (move-char-before text-data)))
  
  (when (key-pressed? :right)
    (cond 
      (and (or (key-down? :left-alt)
             (key-down? :right-alt))
        (or (key-down? :left-shift)
          (key-down? :right-shift)))
      (select-word-after text-data)
      
      (or (key-down? :left-alt)
        (key-down? :right-alt))
      (move-word-after text-data)
      
      (or (key-down? :left-shift)
        (key-down? :right-shift))
      (select-char-after text-data)
      
      (move-char-after text-data)))
  
  (when (key-pressed? :enter)
    (def code (string
                (text-data :text)
                (text-data :selected)
                (string/reverse (text-data :after))))
    (print "Eval! " code)
    (-> (try (do (fiber/setenv (fiber/current) top-env)
                 (set latest-res (string (eval-string code))))
             ([err fib]
              (set latest-res (string "Error: " err))))
      print))
  
  (handle-mouse mouse-data text-data)
  
  (begin-drawing)
  (clear-background (colors :background))
  
  (t/render-textfield conf text-data)
  (draw-text text-conf latest-res [30 120] :blue)
  (end-drawing))

(defn loop-it
  []
  (set loop-fiber
    (ev/call (fn [] (while true
                      (when quit
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

(comment
  (ev/call (fn [] (print (eval-string "(+ 1 1)"))))
  )

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



(comment
#    Vector2 MeasureTextEx(Font font, const char *text, float fontSize, float spacing);
  
  (measure-text-ex font text 40 2)
  
  (measure-text-ex font "abc" 40 2)
  
  (measure-text-ex font "a" 40 2)
  
  (measure-text-ex font "a" 40 2)
  (measure-text-ex font " " 40 2)
  (measure-text-ex font "aa " 40 2)
  (measure-text-ex font "a" 40 2)
  
  (measure-text-ex font "aabbcc" 40 2)
  
  (measure-text-ex font "aabbccddee" 40 2)
  
  (do  (def sii "aab   bccd    dee aoeaoe ")
       (var aoe @[])
       (loop [i :range [0 (length sii)]
              :let [s2 (string/slice sii i (inc i)) 
                    w2 (first (measure-text-ex font s2 40 2))]]
         (array/push aoe w2))
       (print (+ ;aoe))
       (print
         (length sii) " "
         (/
           (- (first (measure-text-ex font sii 40 2)) (+ ;aoe))
           2))
       )
  
  
  (measure-text-ex font "a" 40 2)
  (measure-text-ex font "b" 40 2)
  (measure-text-ex font "c" 40 2)
  
  (+ 26 14 14 11 11 )
  
  
  )

