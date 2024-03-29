(use freja-jaylib)
(import spork/test)
(use ./find_row_etc)

(defn content
  "Returns a big string of all the pieces in the text data."
  [{:selected selected :text text :after after}]
  (string text selected (string/reverse after)))

(defn measure-text
  [tc text]
  (measure-text-ex (tc :font)
                   text
                   (math/floor (* (tc :mult) (tc :size)))
                   (* (tc :mult) (tc :spacing))))

(defn measure-text*
  [font text size spacing]
  (measure-text-ex font
                   text
                   (math/floor (* 1 size))
                   (* 1 spacing)))

(defn measure-text*2
  [font text size spacing scale]
  (measure-text-ex font
                   text
                   (math/floor (* scale size))
                   (* scale spacing)))

(defn draw-text*
  [font text pos size spacing color]
  (draw-text-ex font
                text
                pos
                (math/floor (* 1 size))
                (* 1 spacing)
                color))

(defn draw-text*2
  [font text pos size spacing color scale]
  (draw-text-ex font
                text
                pos
                (math/floor (* scale 1 size))
                (* scale 1 spacing)
                color))

(defn get-pos-in-text
  [text-data x]
  (def {:conf conf :offset offset :text text :selected selected :after after} text-data)
  (def {:size font-size :spacing spacing} conf)

  (var total-w 0)
  (var last -10000)

  (def both (string text selected (string/reverse after)))

  (var res (length both)) # no matches = far right

  (if (<= x offset)
    0
    (do (loop [i :range [0 (length both)]
               :let [s2 (string/slice both i (inc i))
                     [w2 h] (measure-text conf s2)
                     w3 (math/ceil (/ w2 2))]]
          (set total-w (+ total-w w2))

          # (draw-line-ex
          #  [(- (+ offset total-w) w2) 60]
          #  [(- (+ offset total-w) w2) (+ 60 (* h 0.75))]
          #  1
          #  (colors :caret))            

          # (draw-line-ex
          #  [(- (+ offset total-w) w3) 60]
          #  [(- (+ offset total-w) w3) (+ 60 (* h 0.75))]
          #  1
          #  :blue)

          # (draw-line-ex
          #  [(+ offset total-w) 60]
          #  [(+ offset total-w) (+ 60 (* h 0.75))]
          #  1
          #  (colors :caret))

          (when (and (<= x (- (+ total-w offset) w3))
                     (>= x last))
            (set res i)
            (break))

          (set last (- total-w w3))

          (if (not= i (dec (length both)))
            (set total-w (+ total-w spacing))))
      res)))

(defn split-words
  [t]
  (peg/match '(any (+ (capture (some (if-not :s 1)))
                      (capture (* (if-not "\n" :s)))
                      (capture "\n"))) t))

(comment
  (test/timeit (split-words s1000))
  (test/timeit (split-words s100))

  (test/timeit (split-words org-str)))

(defn measure-each-char
  [conf whole-text]
  (def {:size size :spacing spacing} conf)
  (var arr (buffer/new 1))
  (seq [i :range [0 (length whole-text)]
        :let [c (whole-text i)]]
    (put arr 0 c)
    (if (= c (first "\n"))
      (let [[x y] (measure-text conf " ")]
        [0 y])
      (let [[x y] (measure-text conf arr)]
        [(+ x (* (conf :mult) spacing)) y]))))

(defn size-between
  [sizes word]
  (var size @{:w 0 :h 0})
  (loop [c :in word
         :let [[w h] (sizes c)]]
    (-> (update size :w + w)
        (update :h max h)))
  size)

(defn index-before-max-width
  [chars sizes start stop max-width]
  (var ret start)
  (var acc-w 0)
  (loop [i :range [start stop]
         :let [c (chars i)
               [w h] (sizes c)]]
    (+= acc-w w)
    (when (> acc-w max-width)
      (set ret i)
      (break)))
  ret)

(defn regular-word
  [state word {:w w}]
  (let [row (last (state :rows))]
    (-> row
        (put :stop (+ (row :stop) (length word)))
        (update :w + w)
        (update :words array/push word))))

(defn handle-newline
  [state word {:w w :h h}]
  (let [row (last (state :rows))
        start (row :stop)
        stop (+ start (length word))
        new-y (+ (row :y) (* (row :h) (get-in state [:conf :line-height])))]

    (array/push (row :words) word)
    (put row :stop stop)

    (update state :rows array/push @{:y new-y
                                     :h h
                                     :w 0
                                     :words @[]
                                     :start stop
                                     :stop stop})))

(defn add-word [w i] nil)

(defn handle-wide-word
  [state word {:h h}]
  (let [row (last (state :rows))
        start (row :stop)
        stop (+ start (length word))
        i (index-before-max-width word (state :sizes) start stop (state :max-width))
        p (- i start)]

    (loop [word :in [(string/slice word 0 p) (string/slice word p)]
           :let [row (last (state :rows))
                 new-y (+ (row :y) (* (row :h) (get-in state [:conf :line-height])))
                 start (row :stop)]]

      (when (not (empty? (row :words)))
        (put row :word-wrapped true)
        (update state :rows array/push @{:y new-y
                                         :h h
                                         :w 0
                                         :words @[]
                                         :start start
                                         :stop start}))

      (add-word state word))

    state))

(defn handle-wide-line
  [state word size]
  (let [{:w w :h h} size
        {:rows rows
         :max-width max-width} state
        row (last rows)
        new-y (+ (row :y) (* (row :h) (get-in state [:conf :line-height])))
        start (row :stop)]

    #(put row :word-wrapped true)

    (cond (> w max-width)
      (do
        ## TODO: Handle wide word

        :ok
        #(handle-wide-word state word size)
        )

      (not (empty? (row :words)))
      (do (put row :word-wrapped true)
        (-> state
            (update :rows array/push @{:y new-y
                                       :h h
                                       :w 0
                                       :words @[]
                                       :start start
                                       :stop start})
            (add-word word)))

      (add-word state word))))

(defn add-word [state word]
  (let [{:rows rows
         :max-width max-width} state

        row (last rows)

        start (row :stop)

        stop (+ start (length word))
        size (size-between (state :sizes) word)

        {:w w :h h} size
        curr-w (+ (row :w) w)]

    (update row :h max h)

    (cond (= word "\n")
      (handle-newline state word size)

      (> curr-w max-width)
      (handle-wide-line state word size)

      (regular-word state word size))))

(def newline (first "\n"))
(def space (first " "))

## TODO -- make this work

## Need to find either a "real" newline outside of screen -- then it's safe
## or keep going for x amount of lines and give up
## all word wrapped lines that come after a real newline must be recalculated "properly"

(defn words-before-cursor-until-invisible
  "Find all words, and start wrapping them, assuming that the current position is at the end of the current line."
  [sizes width top-y x y chars]

  (var state @{:x x
               :y y
               :i (dec (length chars))
               :word @""
               :words @[]
               :line @{:h 0
                       :logical-newline false}
               :lines @[]

               :width width})

  (defn push-line
    [state newline]
    (put (state :line) :words (state :words))
    (put state :words @[])
    (array/push (state :lines) (state :line))
    (put state :line @{:h 0
                       :logical-newline newline})
    (update state :y - ((sizes (first "a")) 1))
    (put state :x (state :width)))

  (while (and (<= 0 (state :i))
              (> (state :y) top-y))
    (def c (chars (state :i)))
    (case c
      newline (do (array/push (state :words) (string/reverse (state :word)))
                (put state :word @"")
                (push-line state true))
      space (do (array/push (state :words) (string/reverse (state :word)))
              (put state :word @"")
              (do (def new-x (- (state :x) (or (first (sizes c)) 20))) ## TODO: 20 is random number
                (if (neg? new-x)
                  (do (update state :y - ((sizes (first "a")) 1))
                    (put (state :line) :needs-wrapping true)
                    (put state :x (state :width)))
                  (put state :x new-x))

                (update-in state [:line :h] max ((sizes c) 1))
                (buffer/push (state :word) c)))
      (do (def new-x (- (state :x) (or (first (sizes c)) 20))) ## TODO: 20 is random number
        (if (neg? new-x)
          (do (update state :y - ((sizes (first "a")) 1))
            (put (state :line) :needs-wrapping true)
            (put state :x (state :width)))
          (put state :x new-x))

        (update-in state [:line :h] max ((sizes c) 1))
        (buffer/push (state :word) c)))
    (update state :i dec))

  ### we need to keep going for a bit
  # find a line to use as a starting point

  (var found-real-newline false)
  (var limit-lines 5)

  (while (and (not found-real-newline)
              (not (pos? limit-lines)))
    (def c (chars (state :i)))
    (case c
      newline (set found-real-newline true)
      space (update state :x (first (sizes c)))
      (do (def new-x (- (state :x) (first (sizes c))))
        (if (neg? new-x)
          (do (+= limit-lines 1)
            (push-line state false))
          (put state :x new-x))

        (update-in state [:line :h] max ((sizes c) 1))
        (buffer/push (state :word) c)))
    (update state :i dec))

  (when (not (empty? (state :word)))
    (array/push (state :words) (string/reverse (state :word)))
    (push-line state false))

  state

  #(pp state)
  )

(defn words-before-cursor-until-invisiblev1.5
  "Find all words, and start wrapping them, assuming that the current position is at the end of the current line."
  [sizes width top-y x y chars]

  (var x x)
  (var y y)
  (var i (dec (length chars)))
  (var word @"")
  (var words @[])
  (var line @{:h 0 :logical-newline false})
  (var lines @[])

  (defn push-line
    [newline]
    (put line :words words)
    (set words @[])
    (array/push lines line)
    (set line @{:h 0
                :logical-newline newline})
    (set y (- y ((sizes (first "a")) 1)))
    (set x width))

  (while (and (<= 0 i)
              (> y top-y))
    (def c (chars i))
    (case c
      newline (do (array/push words (string/reverse word))
                (set word @"")
                (push-line true))
      space (do (array/push words (string/reverse word))
              (set word @"")
              (def [w h] (sizes c))
              (do (def new-x (- x (or w 20))) ## TODO: 20 is random number
                (if (neg? new-x)
                  (do (set y (- y ((sizes (first "a")) 1)))
                    (put line :needs-wrapping true)
                    (set x width))
                  (set x new-x))

                (update line :h max 1)
                (buffer/push word c)))

      (let [[w h] (sizes c)]
        (def new-x (- x (or w 20))) ## TODO: 20 is random number
        (if (neg? new-x)
          (do (set y (- y ((sizes (first "a")) 1)))
            (put line :needs-wrapping true)
            (set x width))
          (set x new-x))

        (update line :h max h)
        (buffer/push word c)))
    (-= i 1))

  (when (not (empty? word))
    (array/push words (string/reverse word))
    (push-line false))

  lines

  #(pp state)
  )

(var lines @[])
(var need-wordwrap @[])

(defn words-before-cursor-until-invisiblev2
  "Find all words, and start wrapping them, assuming that the current position is at the end of the current line."
  [lines
   need-wordwrap
   sizes width top-y x y chars]

  (array/clear lines)
  (array/clear need-wordwrap)

  (var should-be-wrapped false)

  (var x x)
  (var y y)

  (def h ((sizes (first "a")) 1))

  (loop [i :down-to [(dec (length chars)) 0]
         :let [c (chars i)]
         :while (> y top-y)]
    (if (= newline c)
      (do (-= y h)
        (when should-be-wrapped
          (array/push need-wordwrap i)
          (set should-be-wrapped false))
        (array/push lines i))

      (do (def new-x (- x (first (sizes c))))
        (if (neg? new-x)
          (do (-= y h)
            (set should-be-wrapped true)
            #(array/push need-wordwrap i)
            (set x width))
          (set x new-x)))))

  lines)

(defn wordwrap2
  [conf sizes words max-width]
  (var rows @[@{:y 0
                :h 0
                :w 0
                :words @[]
                :start 0
                :stop 0}])

  (def state @{:conf conf
               :max-width max-width
               :sizes sizes
               :rows rows})

  (loop [word :in words]
    (add-word state word))

  rows)

(defn wordwrap2-max-y
  [conf sizes text max-width max-y]
  (var rows @[@{:y 0
                :h 0
                :w 0
                :words @[]
                :start 0
                :stop 0}])

  (def state @{:conf conf
               :max-width max-width
               :sizes sizes
               :rows rows})

  (var i (dec (length text)))
  (var word @"")

  (loop [i :down-to [(dec (length text)) 0]
         :while (> max-y ((last rows) :y))
         :let [c (text i)]]
    (case c
      newline (do (when (not (empty? word))
                    (add-word state (buffer word)))
                (add-word state "\n")
                (buffer/clear word))
      space (do (when (not (empty? word))
                  (add-word state (buffer word)))
              (buffer/clear word)
              (add-word state " "))
      (buffer/push word c)))

  rows)

(defn glyphs->size-struct
  [conf glyphs]
  (table ;(interleave
            glyphs
            (measure-each-char conf glyphs))))

(comment

  (wordwrap2-max-y
    (text-data :conf)
    (text-data :sizes)
    (string/reverse org-str)
    100
    400)

  (test/timeit (split-words2 org-str))
  (test/timeit (split-words org-str))

  (def glyphs ((text-data :conf) :glyphs))
  (def sizes
    (table ;(interleave
              glyphs
              (measure-each-char (text-data :conf) glyphs))))

  (dyn :pretty-format "%.4m")
  (with-dyns [:pretty-format "%.4m"]
    (pp sizes))

  (string/format "%.4m" sizes)

  (test/timeit (split-words org-str))

  (def string-thing ``
   a
   htsneh oastnhe oastohaens etnsoa esnhtoa snheao hhhhhhhhhhhhhhhhhhh hhhhhhhhhhhhhhhh hhhhhhhhhhhhhhh hhhhhhhhhhh
   b
   c
   ``)

  (def string-thing s100)

  (def nrows (do
               (def got-state (words-before-cursor-until-invisible
                                sizes
                                100
                                -500
                                300
                                0
                                string-thing))

               (def huh (do (def lines @[])
                          (loop [l :in (drop 1 (got-state :lines))]
                            (if (l :needs-wrapping)
                              (array/concat lines
                                            (wordwrap2
                                              (text-data :conf)
                                              sizes
                                              (l :words)
                                              100))
                              (array/push lines l)))
                          lines))))

  (def old-sizes (measure-each-char (text-data :conf) string-thing))

  (test/timeit (old/wordwrap
                 (text-data :conf)
                 old-sizes
                 (split-words string-thing)
                 100))

  ((first huh) :words)

  (huh 1)

  (map |($ :words) huh)

  (array/concat @[1]

                (wordwrap2
                  (text-data :conf)
                  sizes
                  ((first (filter |($ :needs-wrapping) (drop 1 (got-state :lines)))) :words)
                  100))

  (length (test/timeit
            (split-words (buffer/slice
                           s1000
                           ((words-before-cursor-until-invisible
                              sizes
                              300
                              -500
                              300
                              0
                              s1000) :i)

                           (length s1000)))))

  # Elapsed time: 0.00181174 seconds
  (test/timeit (words-before-cursor-until-invisible
                 sizes
                 300
                 -500
                 300
                 0
                 s1000))

  (test/timeit (words-before-cursor-until-invisible
                 sizes
                 300
                 -500
                 300
                 0
                 org-str))

  (map identity @``b
       a``)

  (map identity @"b
       a")

  (test/timeit (words-before-cursor-until-invisible
                 sizes
                 300
                 -500
                 300
                 0
                 s100)))

(comment
  (def got-state (words-before-cursor-until-invisible
                   (text-data :sizes)
                   100
                   -500
                   300
                   0
                   (text-data :text))))


(defn char-positions
  [sizes rows]
  (var i 0)
  (var ps @[])
  (loop [{:y y-pos :words words} :in rows
         :let [nof (+ ;(map length words))
               stop (+ i nof)]]
    (var acc-x 0)
    (for i2 i stop
      (def [x y] (sizes i2))
      (put ps i2 {:x acc-x :y-hehe y-pos
                  :center-x (+ acc-x (* x 0.5))})
      (set acc-x (+ acc-x x)))
    (+= i nof))
  ps)

(defn y->row
  "Finds the row closest to a y position."
  [{:rows rows :offset offset} y]
  (binary-search-closest rows |(compare y (+ ($ :y) ($ :h) (offset 0)))))

(defn pos->row
  "Figures out the de facto row of a logical position."
  [{:rows rows :offset offset} pos]
  (rows (binary-search-closest rows (fn [{:start start :stop stop}]
                                      (cond (> start pos) # pos too small
                                        -1

                                        (<= stop pos) # pos too big
                                        1

                                        0))))) # pos between start and stop :)

(comment

  (pp (text-data :rows))

  (pos->row text-data 0)
  (pos->row text-data 4)
  ((pos->row text-data 0) :y))

(defn range->rects
  [props start stop]
  (def {:positions ps :sizes sizes} props)
  (var rects @[])
  (loop [i :range [start stop]
         :let [{:x x} (ps i)
               y ((pos->row props i) :y)
               [w h] (sizes i)
               r (last rects)]]
    (if (= (get r :y) y)
      (-> r
          (put :w (- (+ x w) (r :x)))
          (update :h max h))
      (array/push rects @{:x x :y y :w w :h h})))
  ## TODO Don't depend on :y
  rects)

(comment
  (range->rects
    (text-data :positions)
    (text-data :sizes)
    0
    5))

(defn get-caret-pos
  [{:current-row current-row
    :offset offset
    :rows rows
    :positions ps
    :sizes sizes
    :conf text-conf
    :text text
    :stickiness stickiness}]
  (def [ox oy] offset)
  (def {:size font-size
        :spacing spacing} text-conf)

  (let [newline (= (first "\n") (last text))
        word-wrapped-down (and (= stickiness :down)
                               (get-in rows [(dec current-row) :word-wrapped])
                               (= (length text) (get-in rows [(dec current-row) :stop])))
        word-wrapped-right (and (= stickiness :right)
                                (get-in rows [current-row :word-wrapped])
                                (= (length text) (get-in rows [current-row :stop])))
        y ((get rows current-row) :y)]

    (when-let [{:x cx :y cy} (or (when-let [pos (get ps (max (dec (length text)) 0))]
                                   (cond newline
                                     (let [h (* ((get rows current-row) :h)
                                                (text-conf :line-height))]
                                       {:x 0 :y (+ y h)})

                                     word-wrapped-down
                                     (let [h (* ((get rows current-row) :h)
                                                (text-conf :line-height))]
                                       {:x 0 :y (+ y h)})

                                     word-wrapped-right
                                     {:x (pos :x) :y y}

                                     {:x (pos :x) :y y}))
                                 {:x 0 :y 0})]
      (let [s (get sizes (dec (length text)))
            w (if (or newline word-wrapped-down) 0 (get s 0 0))]
        [(- (+ cx w) (* spacing 1.25)) cy]))))

(defn row-of-pos
  "Row of logical character index."
  [rows pos]
  (var current-row 0)
  (loop [i :range [0 (length rows)]
         :let [r (rows i)]]
    (when (and (>= (max pos 0) (r :start))
               (< (max pos 0) (r :stop)))
      (set current-row i)
      (break))

    ## it's the last, empty row
    (set current-row i))
  current-row)

(defn cursor-pos
  "Returns the position of the cursor, which depends on the direction of selection."
  [props]
  (if (= :right (props :dir))
    (+ (length (props :text))
       (length (props :selected)))
    (length (props :text))))

(defn weighted-row-of-pos
  "Calculates the row of the position `cp`.
Takes stickiness into account,
e.g. when at the end / right after a word wrapped line."
  [props cp]
  (def {:rows rows} props)
  (def all-text (content props))
  (when rows
    (var current-row 0)
    (loop [i :range [0 (length rows)]
           :let [r (rows i)]]
      (when (and (>= (max (dec cp) 0) (r :start))
                 (< (max (dec cp) 0) (r :stop)))
        (set current-row i)
        (break))

      ## it's the last, empty row
      (set current-row i))

    (when (and (not (empty? all-text))
               (= (first "\n")
                  (get all-text (dec cp))
                  #(last text)
                  ))
      (+= current-row 1))

    (when (and (get-in rows [current-row :word-wrapped])
               (= cp (get-in rows [current-row :stop]))
               (= (props :stickiness) :down))
      (+= current-row 1))

    current-row))

(defn refresh-caret-pos
  [props]
  (comment
    ## TODO: Uncomment
    (re-measure props)
    (put props :caret-pos (get-caret-pos props))))

(defn reset-blink
  [props]
  (set (props :blink) 0)
  props)

(defn scroll-to-focus
  [props]
  (comment
    # TODO: Uncomment this stuff
    (let [curr-y (get ((props :rows) (props :current-row)) :y 0)
          curr-h (get ((props :rows) (props :current-row)) :h 0)]
      # cursor too far down
      (when (> (+ curr-y (* 2 curr-h))
               (+ (- (props :scroll)) (props :calculated-h)))
        (put props :scroll (- (- (+ curr-y curr-h) (* 0.5 (props :calculated-h))))))

      # cursor too far up
      (when (< curr-y (- (props :scroll)))
        (put props :scroll (- (- curr-y (* 0.5 (props :calculated-h)))))))))
