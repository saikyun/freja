# run `janet main.janet`
# in another terminal, run `janet main.janet connect`
# then modify and eval the `(varfn frame ...)` form

(use ./build/jaylib)
(import spork/netrepl)

(var top-env (fiber/getenv (fiber/current)))

(var font nil)
(var loop-fiber nil)
(var text @"")
(var selected @"")
(var selected-left-right :left)
(var text-after @"")

(var latest-res @"aoe")

(set latest-res @"hmm")

(def colors
  {:text       [0.396 0.478 0.514]
   :border     [0.396 0.478 0.514]
   :background [0.992 0.965 0.89]
   :selected-text [0.992 0.965 0.89]
   :selected-text-background :blue
   :caret      [0.396 0.478 0.514]})

(varfn get-pos-in-text
  [text x offset font-size spacing]
  (var total-w 0)        
  (var last -10000)        
  (var res (length text)) # no matches = far right
  
  (if (<= x offset)
    0
    (do (loop [i :range [0 (length text)]
               :let [s2 (string/slice text i (inc i)) 
                     [w2 h] (measure-text-ex font s2 font-size spacing)
                     w3 (math/ceil (/ w2 2))]]
          (set total-w (+ total-w w2))
          
          ## (draw-line-ex
          ##   [(- (+ offset total-w) w2) 60]
          ##   [(- (+ offset total-w) w2) (+ 60 (* h 0.75))]
          ##   1
          ##   (colors :caret))            
          
          ## (draw-line-ex
          ##   [(+ offset total-w) 60]
          ##   [(+ offset total-w) (+ 60 (* h 0.75))]
          ##   1
          ##   (colors :caret))
          
          ## (draw-line-ex
          ##   [(- (+ offset total-w) w3) 60]
          ##   [(- (+ offset total-w) w3) (+ 60 (* h 0.75))]
          ##   1
          ##   :blue)
          
          (when (and (<= x (- (+ total-w offset) w3))
                  (>= x last))
            (set res i)
            (break))
          
          (set last (- total-w w3))
          
          (if (not= i (dec (length text)))
            (set total-w (+ total-w spacing))))
        res)))

(var mouse-just-down nil)
(var mouse-just-double-clicked nil)
(var mouse-just-triple-clicked nil)
(var mouse-recently-double-clicked nil)
(var mouse-recently-triple-clicked nil)
(var mouse-down-pos   nil)
(var mouse-down-time  nil)
(var mouse-down-time2 nil)
(var mouse-up-pos     nil)
(var selected-pos     nil)
(var last-text-pos    nil)

(defn select-until-beginning
  "Selects all text from cursor to beginning of buffer."
  []
  (set selected-left-right :left)
  (buffer/push-string selected text)
  (buffer/clear text))

(defn select-until-end
  "Selects all text from cursor to end of buffer."
  []
  (set selected-left-right :right)
  (buffer/push-string selected (string/reverse text-after))
  (buffer/clear text-after))

(defn move-to-beginning
  "Moves cursor to beginning of buffer."
  []
  (buffer/push-string text-after (string/reverse selected))
  (buffer/push-string text-after (string/reverse text))
  (buffer/clear selected)
  (buffer/clear text))

(defn move-to-end
  "Moves cursor to end of buffer."
  []
  (buffer/push-string text selected)
  (buffer/push-string text (string/reverse text-after))
  (buffer/clear selected)
  (buffer/clear text-after))

(defn copy
  "Copies selected text into clipboard."
  []
  (set-clipboard-text (string selected)))

(defn delete-selected
  "Deletes selected text.
  Always run when inserting (e.g. writing chars or when pasting).
  Returns previously selected text.
  Returns `nil` if no text was selected."
  []
  (def old selected)
  (set selected @"")
  (when (not (empty? old))
    old))

(defn cut
  "Cuts selected text into clipboard."
  []
  (set-clipboard-text (string selected))
  (delete-selected))

(defn paste
  "Pastes from clipboard."
  []
  (delete-selected)
  (buffer/push-string text (get-clipboard-text)))

(defn select-all
  "Selects all text in buffer."
  []
  (def new-selected (buffer/new (+ (length text)
                                  (length selected)
                                  (length text-after))))    
  (buffer/push-string new-selected text)    
  (buffer/push-string new-selected selected)    
  (buffer/push-string new-selected (string/reverse text-after))    
  (set selected new-selected)    
  (buffer/clear text)    
  (buffer/clear text-after))

(defn delete-word-before
  "Deletes the word before the cursor.
  If text is selected, deletes the selection instead."
  []
  (when (not (delete-selected))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text)))]
      (buffer/popn text l))))

(defn delete-word-after
  "Deletes the word after the cursor.
  If text is selected, deletes the selection instead."
  []
  (when (not (delete-selected))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text-after)))]
      (buffer/popn text-after l))))

(defn backspace
  "Removes a single character before the cursor.
  If text is selected, deletes the selection instead."
  []
  (when (not (delete-selected))
    (buffer/popn text 1)))

(defn forward-delete
  "Removes a single character after the cursor.
  If text is selected, deletes the selection instead."
  []
  (when (not (delete-selected))
    (buffer/popn text-after 1)))

(defn select-word-before
  "Selects a word before the cursor."
  []
  (if (and (not (empty? selected))       # when text is selected, and the direction is right
        (= selected-left-right :right))  # we deselect rather than select
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse selected)))]
      (buffer/push-string text-after (string/reverse (buffer/slice selected (dec (- l)))))
      (buffer/popn selected l))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text)))]
      (set selected-left-right :left)
      (set selected (buffer (buffer/slice text (dec (- l))) selected))
      (buffer/popn text l))))

(defn select-word-after
  "Selects a word after the cursor."
  []
  (if (and (not (empty? selected))     # when text is selected, and the direction is left
        (= selected-left-right :left)) # we deselect rather than select
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) selected))]
      (buffer/push-string text (buffer/slice selected 0 l))
      (set selected (buffer/slice selected l)))
    (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text-after)))]
      (set selected-left-right :right)
      (buffer/push-string selected (string/reverse (buffer/slice text-after (dec (- l)))))
      (buffer/popn text-after l))))

(defn move-word-before
  "Moves the cursor one word to the left."
  []
  (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text)))]
    (when (not (empty? selected))
      (buffer/push-string text-after (string/reverse selected))
      (buffer/clear selected))
    (buffer/push-string text-after (string/reverse (buffer/slice text (dec (- l)))))
    (buffer/popn text l)))

(defn move-word-after
  "Moves the cursor one word to the right."
  []
  (when-let [l (first (peg/match '(* (any :s) (any :S) ($)) (string/reverse text-after)))]
    (when (not (empty? selected))
      (buffer/push-string text selected)
      (buffer/clear selected))
    (buffer/push-string text (string/reverse (buffer/slice text-after (dec (- l)))))
    (buffer/popn text-after l)))

(defn select-char-before
  "Selects the char before the cursor."
  []
  (if (and (= selected-left-right :right)
        (not (empty? selected)))
    (do (put text-after (length text-after) (last selected))
        (buffer/popn selected 1))
    (when (not (empty? text))
      (set selected-left-right :left)
      (let [o selected]
        (set selected (buffer/new (inc (length o))))
        (put selected 0 (last text))                    
        (buffer/push-string selected o))
      (buffer/popn text 1))))

(defn select-char-after
  "Selects the char after the cursor."
  []
  (if (and (= selected-left-right :left)
        (not (empty? selected)))
    (do (put text (length text) (first selected))
        (set selected (buffer/slice selected 1)))
    (when (not (empty? text-after))
      (set selected-left-right :right)
      (put selected (length selected) (last text-after))
      (buffer/popn text-after 1))))

(defn move-char-before
  "Moves the cursor one char to the left."
  []
  (if (not (empty? selected))
    (do (buffer/push-string text-after (string/reverse selected))
        (buffer/clear selected))
    (when (not (empty? text))
      (put text-after (length text-after) (last text))
      (buffer/popn text 1))))

(defn move-char-after
  "Moves the cursor one char to the right."
  []
  (if (not (empty? selected))
    (do (buffer/push-string text selected)
        (buffer/clear selected))
    (when (not (empty? text-after))
      (put text (length text) (last text-after))
      (buffer/popn text-after 1))))

(defn insert-char
  "Inserts a single char."
  [k]
  (case k
    :space (buffer/push-string text " ")
    :grave (buffer/push-string text "`")
    (do (buffer/clear selected)
        (if (keyword? k)
          (buffer/push-string text (string k))
          (put text (length text) k)))))

(varfn frame
  []
  (var k (get-key-pressed))
  
  (while (not= 0 k)
    (insert-char k)
    (set k (get-key-pressed)))
  
  (when (key-pressed? :home)
    (if (or (key-down? :left-shift)
          (key-down? :right-shift))
      (select-until-beginning)
      (move-to-beginning)))
  
  (when (key-pressed? :end)
    (if (or (key-down? :left-shift)
          (key-down? :right-shift))
      (select-until-end)
      (move-to-end)))
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :.))
    (paste))
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :a))
    (select-all))
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :i))
    (copy))
  
  (when (and (or (key-down? :left-super)
               (key-down? :right-super))
          (key-pressed? :b))
    (cut))
  
  (when (key-pressed? :backspace)
    (cond (or (key-down? :left-alt)
            (key-down? :right-alt))
          (delete-word-before)
          
          (backspace)))
  
  (when (key-pressed? :delete)
    (cond (or (key-down? :left-alt)
            (key-down? :right-alt))
          (delete-word-after)
          
          (forward-delete)))
  
  (when (key-pressed? :left)
    (cond
      ## select whole words
      (and (or (key-down? :left-alt)
             (key-down? :right-alt))
        (or (key-down? :left-shift)
          (key-down? :right-shift)))
      (select-word-before)
      
      (or (key-down? :left-alt)
        (key-down? :right-alt)) 
      (move-word-before)
      
      (or (key-down? :left-shift)
        (key-down? :right-shift))
      (select-char-before)
      
      (move-char-before)))
  
  (when (key-pressed? :right)
    (cond 
      (and (or (key-down? :left-alt)
             (key-down? :right-alt))
        (or (key-down? :left-shift)
          (key-down? :right-shift)))
      (select-word-after)
      
      (or (key-down? :left-alt)
        (key-down? :right-alt))
      (move-word-after)
      
      (or (key-down? :left-shift)
        (key-down? :right-shift))
      (select-char-after)
      
      (move-char-after)))
  
  (when (key-pressed? :enter)
    (print "Eval! " (string text (string/reverse text-after)))
    (-> (try (do (def code (string text selected (string/reverse text-after)))
                 (fiber/setenv (fiber/current) top-env)
                 (set latest-res (string (eval-string code))))
             ([err fib]
              (set latest-res (string "Error: " err))))
      print))
  
  (begin-drawing)
  (clear-background (colors :background))
  (let [[x y] (get-mouse-position)]
    
#     void DrawRectangleRounded(Rectangle rec, float roundness, int segments, Color color);
#     Draw rectangle with rounded edges
    
    (let [x 10
          y 35
          w 300
          h 40
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
    
    (set mouse-just-double-clicked false)
    (set mouse-just-triple-clicked false)
    
    (let [font-size 40
          left-margin 30
          spacing 2
          both (string text selected (string/reverse text-after))]
      
#(print x " " y)
      (when (mouse-button-released? 0)
        (set last-text-pos nil)
        (set mouse-just-down nil)
        (set mouse-recently-double-clicked nil)
        (set mouse-recently-triple-clicked nil)
        (set mouse-up-pos [x y])
        
        (set selected-pos [(get-pos-in-text
                             both
                             (first mouse-down-pos)
                             left-margin
                             font-size
                             spacing)
                           (get-pos-in-text
                             both
                             x
                             left-margin
                             font-size
                             spacing)]))
      
      (when (mouse-button-pressed? 0)
        (when mouse-down-time2
          (print  (- (get-time) mouse-down-time2)))
        (when (and mouse-down-time2
                (> 0.4 (- (get-time) mouse-down-time2)))
          (set mouse-just-triple-clicked true)
          (set mouse-recently-triple-clicked true))
        
        (when mouse-down-time
          (print "huh "  (- (get-time) mouse-down-time)))
        (when (and mouse-down-time
                (> 0.25 (- (get-time) mouse-down-time)))
          (set mouse-just-double-clicked true)
          (set mouse-recently-double-clicked true)
          (set mouse-down-time2 (get-time))))
      
      (cond mouse-just-triple-clicked 
            (do (buffer/clear text)
                (buffer/clear text-after)
                (set selected both)
                
                (set selected-left-right :right))
            
            (and mouse-just-double-clicked
              (not (key-down? :left-shift))
              (not (key-down? :right-shift))) 
            (do (buffer/push-string text selected)
                (buffer/clear selected)                
                
                (def t-l (first (peg/match '(* (any :S) ($)) (string/reverse text))))
                (def at-l (first (peg/match '(* (any :S) ($)) (string/reverse text-after))))
                
                (buffer/push-string selected (string/slice text (dec (- t-l))))
                (buffer/push-string selected (string/reverse (string/slice text-after (dec (- at-l)))))
                (buffer/popn text t-l)
                (buffer/popn text-after at-l)
                
                (set selected-left-right :right))
            
            (or mouse-recently-double-clicked
              mouse-recently-triple-clicked) nil # don't start selecting until mouse is released again
            
            (mouse-button-down? 0)
            (do (when (nil? last-text-pos)
                  (set last-text-pos (length text)))
                
                (set mouse-down-time (get-time))
                (if (= nil mouse-just-down)
                  (do (set mouse-just-down true)
                      (set mouse-down-pos [x y]))
                  (set mouse-just-down false))
                
                (set selected-pos [(get-pos-in-text
                                     both
                                     (first mouse-down-pos)
                                     left-margin
                                     font-size
                                     spacing)
                                   (get-pos-in-text
                                     both
                                     x
                                     left-margin
                                     font-size
                                     spacing)])
                
                (var moved-caret false)
                
                (let [[start end] selected-pos
                      start (if (or (key-down? :left-shift)
                                  (key-down? :right-shift))
                              last-text-pos
                              start)
                      [start end] (if (> start end)
                                    (do (set selected-left-right :left)
                                        [end start])
                                    (do (set selected-left-right :right)
                                        [start end]))]
                  (set text (buffer/slice both 0 start))
                  (set selected (buffer/slice both start end))
                  (buffer/clear text-after)
                  (buffer/push-string text-after (string/reverse (buffer/slice both end))))))
      
      (let [[w h] (measure-text-ex font text font-size spacing)
            [w2 h2] (measure-text-ex font selected font-size spacing)]
        (draw-text-ex font text [30 30] font-size spacing (colors :text))
        
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
          (string/reverse text-after)
          [(+ 30 spacing w spacing w2) 30]
          font-size spacing (colors :text))

        (draw-line-ex [(+ 30 spacing w) font-size] [(+ 30 spacing w) (+ font-size (* h 0.75))] 1 (colors :caret)))
      
      (draw-text-ex font latest-res [30 80] font-size spacing :blue))
    
#     void DrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color color)
    
    
    
    )
  (end-drawing))

(defn loop-it
  []
  (set loop-fiber
    (ev/call (fn [] (while true
                      (when (key-pressed? :q)
                        (close-window)
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
    (do (init-window 800 600 "Textfield")
        
        (set font (load-font-ex "Texturina-VariableFont_opsz,wght.ttf" 40
# =?\"'!|,*:
                    (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäååæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ")))
        
        (set-target-fps 60)
        
        (loop-it))
    ([err fib]
     (print "error! " err)
     (close-window))))

(def env (fiber/getenv (fiber/current)))


(defn main [& args]
  
  (if (= (get args 1) "connect")
    (netrepl/client "127.0.0.1" "9365" "bob")
    (do (netrepl/server "127.0.0.1" "9365" env)
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

