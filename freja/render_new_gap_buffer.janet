(import spork/test)
(use freja-jaylib)
(import ./dumb :prefix "")
(import ./new_gap_buffer :prefix "")
(import ./textfield_api :prefix "")
(import ./text_rendering :as tr)
(import ./find_row_etc :prefix "")
(import freja/assets :as a)
(import ./rainbow :as rb :fresh true)
(import ./highlighting :as hl :fresh true)

(setdyn :freja/ns "freja/render_new_gap_buffer")

(use profiling/profile)

(defn reset-blink
  [props]
  (set (props :blink) 0)
  props)

(varfn height
  [{:position position
    :offset offset
    :size size}]
  (def [_ y] position)
  (def [_ oy] offset)
  (def [_ h] size)

  (if (= h :max)
    (- (get-screen-height)
       y
       oy)
    h))

(varfn inner-width
  [{:position position
    :offset offset
    :size size}]
  (def [x _] position)
  (def [ox _] offset)
  (def [w _] size)

  (if (= w :max)
    (- (get-screen-width)
       x
       ox)
    w))

(varfn width
  [{:position position
    :offset offset
    :size size}]
  (def [x _] position)
  (def [ox _] offset)
  (def [w _] size)

  (if (= w :max)
    (- (get-screen-width)
       x)
    w))

(def warned-chars @{})

(varfn get-size
  [sizes c]
  (let [sz (get sizes c)]
    (if-not sz
      (let [sz (first (values sizes))]
        (unless (warned-chars c)
          (print "no size for char " c ", using first sizes instead." sz)
          (put warned-chars c true))
        #(debug/stacktrace (fiber/current))
        sz)
      sz)))

(varfn index-passing-max-width
  "Returns the index of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [gb start stop max-width]

  # (def {:sizes sizes} gb)

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (var acc-w 0)

  (gb-iterate
    gb
    start stop
    i c
    (let [[w h] (get-size sizes c)]
      (+= acc-w w)
      (when (dyn :debug)
        (print "i: " i " - c: " c " - " "acc-w: " acc-w))
      (when (> acc-w max-width) # we went too far!
        (return stop-gb-iterate i)))))

(varfn index-passing-middle-max-width
  "Returns the index of the middle of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [gb start stop max-width]

  (var acc-w 0)
  (var last-w 0)

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (or (gb-iterate
        gb
        start stop
        i c
        (let [[w h] (sizes c)]
          (+= acc-w (+ (* w 0.5) last-w))
          (set last-w (* w 0.5))
          (when (dyn :debug)
            (print "i: " i " - c: " c " - " "acc-w: " acc-w))
          (when (> acc-w max-width) # we went too far!
            (return stop-gb-iterate i))))
      stop))

(comment

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (with-dyns [:debug true]
    (let [gb {:text @"abc"
              :sizes sizes
              :gap-start 0
              :gap-stop 0
              :gap @"123"}]
      (index-passing-max-width gb
                               0 3
                               20))))

(varfn word-wrap-gap-buffer
  [props

   sizes

   lines
   y-poses
   line-flags
   line-numbers

   gb
   start
   start-line

   stop
   width
   h
   y-offset
   y-limit

   &keys {:line-limit line-limit}]

  (default line-limit 999999999999)

  (def old-y-pos (get y-poses start-line 0))
  (def old-line-number (get line-numbers start-line 1))

  # remove everything after start
  # since start is where a change (might) have happened
  (array/remove lines (or start-line 0) (length lines))
  (array/remove y-poses (or start-line 0) (length y-poses))
  (array/remove line-flags (or start-line 0) (length line-flags))
  (array/remove line-numbers (or start-line 0) (length line-numbers))
  #(array/clear lines)
  #(array/clear y-poses)
  #(array/clear line-flags)
  #(array/clear line-numbers)

  (var x 0)
  (var y old-y-pos)
  (var w 0)
  (var line-number old-line-number)

  (var s (buffer/new 1))
  (var beginning-of-word-i 0)

  (def {:offset offset
        :width-of-last-line-number width-of-last-line-number} gb)
  (def [x-scale _] screen-scale)

  (def treshold (- width (offset 0) (or width-of-last-line-number 0)))

  (defn check-if-word-is-too-wide
    [old-w i c]
    (if (> (+ x old-w) treshold) ## we went outside the max width
      (do ## so rowbreak before the word
        (when (not= beginning-of-word-i 0)
          (array/push lines beginning-of-word-i)
          (array/push line-numbers line-number)
          (array/push y-poses y)
          (array/push line-flags :word-wrap)
          (+= y h))
        (set x old-w)

        (when (> (+ x old-w) treshold)
          ## then we need to break up the word
          (var break-i beginning-of-word-i)
          (while (set break-i (-?> (index-passing-max-width
                                     gb
                                     break-i
                                     i
                                     treshold)
                                   dec)) # hotheotehtehomoa
            (array/push lines break-i)
            (array/push line-numbers line-number)
            (array/push y-poses y)
            (array/push line-flags :word-wrap)
            (+= y h))))

      (+= x (+ old-w (first (get-size sizes c))))))

  (gb-iterate
    gb
    start
    stop
    i c
    (case c newline
      (do (check-if-word-is-too-wide w i c)
        (array/push lines i)
        (array/push line-numbers line-number)
        (++ line-number)
        (array/push y-poses y)
        (array/push line-flags :regular)
        (+= y h)
        (set x 0)
        (set w 0)
        (set beginning-of-word-i (inc i)))

      (chr "\r") (do)

      space
      (let [old-w w]
        (set w 0)

        (check-if-word-is-too-wide old-w i c)

        (set beginning-of-word-i (inc i)))

      (let [new-w (+ w (first (get-size sizes c)))]
        (set w new-w)))

    # TODO: this seem to be called WAY too much!
    (when (or (> (+ y y-offset) y-limit)
              (>= line-number line-limit))
      (return stop-gb-iterate)))

  (when (not (> (+ y y-offset) y-limit))
    (let [old-w w]
      (set w 0)

      (if (> (+ x old-w) treshold) ## we went outside the max width
        (do ## so rowbreak before the word
          (when (not= beginning-of-word-i 0)
            (array/push lines beginning-of-word-i)
            (array/push line-numbers line-number)
            (array/push y-poses y)
            (array/push line-flags :word-wrap)
            (+= y h))
          (set x old-w)

          ## is the word also longer than the line?
          (when (> (+ x old-w) treshold)
            ## then we need to break up the word
            (var break-i beginning-of-word-i)
            (while (set break-i (-?> (index-passing-max-width
                                       gb
                                       break-i
                                       stop
                                       treshold)
                                     dec))
              (array/push lines break-i)
              (array/push line-numbers line-number)
              (array/push y-poses y)
              (array/push line-flags :word-wrap)
              (+= y h))))))

    (array/push lines stop)
    (array/push line-numbers line-number)
    (array/push y-poses y)
    (array/push line-flags :regular))

  lines)

(var debug nil)
(set debug true)
(set debug false)

(varfn in-selection?
  [{:selection selection :caret caret} i]
  (when selection
    (let [start (min selection caret)
          stop (max selection caret)]
      (and (>= i start)
           (< i stop)))))

(varfn width-between
  [gb start stop]
  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))
  (def [x-scale y-scale] screen-scale)
  (var acc-w 0)

  (gb-iterate
    gb
    start stop
    i c
    (let [[w h] (get-size sizes c)] (+= acc-w w)))

  acc-w)

(comment
  (width-between gb-data 0 10)

  (gb-iterate
    gb-data
    0 100
    i c
    (print i)))

(varfn render-selection-box
  [gb start stop y]
  (def {:selection selection
        :colors colors
        :caret caret
        :offset offset
        :width-of-last-line-number width-of-last-line-number} gb)
  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))
  (def [x-scale y-scale] screen-scale)

  (when selection
    (let [sel-start (min selection caret)
          sel-stop (max selection caret)]

      (when (and (< sel-start stop)
                 (>= sel-stop start))

        (var line-h (* (gb :text/size) (gb :text/line-height)))
        (var start-x nil)
        (var stop-x nil)
        (var acc-w 0)

        (gb-iterate
          gb
          start stop
          i c
          (let [[w h] (get-size sizes c)]
            (when (and (nil? start-x)
                       (>= i sel-start))
              (set start-x acc-w))

            (+= acc-w (* x-scale w))

            (when (and start-x
                       (< i sel-stop))
              (set stop-x acc-w))

            (set line-h (max line-h h))))

        (when (and start-x
                   stop-x)
          (draw-rectangle-rec
            [(+ (offset 0)
                width-of-last-line-number
                start-x)
             (+ (* y y-scale) (offset 1))
             (- stop-x start-x)
             (* y-scale line-h)]
            (colors :selected-text-background)
            #:blue
))))))

(varfn abs-x
  [gb x]
  (+ ((gb :position) 0)
     x))

(varfn rel-text-x
  [gb x]
  (+ ((gb :offset) 0)
     (gb :width-of-last-line-number)
     x))

(varfn abs-text-x
  [gb x]
  (def [x-scale _] screen-scale)
  (+ ((gb :position) 0)
     (/ ((gb :offset) 0) x-scale)
     (/ (gb :width-of-last-line-number)
        x-scale)
     x))

(varfn abs-text-y
  [gb y]
  (+ ((gb :position) 1)
     ((gb :offset) 1)
     y))

(varfn rel-y
  [gb y]
  (+ ((gb :offset) 1)
     y))

(comment
  (defn measure-text
    [tc text]
    (measure-text-ex (tc :font)
                     text
                     (math/floor (* (tc :mult) (tc :size)))
                     (* (tc :mult) (tc :spacing))))
  #
)

(defn gb-draw-text
  [gb text pos color]
  (def font (a/font (gb :text/font) (gb :text/size)))
  (tr/draw-text*2 font
                  text
                  pos
                  (gb :text/size)
                  (gb :text/spacing)
                  color
                  (screen-scale 0)))

(defn gb-measure-text
  [gb text]
  (def font (a/font (gb :text/font) (gb :text/size)))
  (tr/measure-text*2 font
                     text
                     (gb :text/size)
                     (gb :text/spacing)
                     (screen-scale 0)))


(varfn render-lines
  "Renders the lines in gap buffer.
Also renders selection boxes.
Render lines doesn't modify anything in gb."
  [sizes gb lines start-index h y-limit]
  (def {:delim-ps delim-ps
        :y-poses y-poses
        :line-numbers line-numbers
        :highlighting highlighting
        :offset offset
        :colors colors
        :debug debug} gb)
  (def [x-scale y-scale] screen-scale)
  (def [x-offset y-offset] offset)

  (def default-text-color (colors :text))

  (var delim-i 0)
  (var hl-i 0)

  (when debug
    (print "rendering lines")
    (print "from " start-index " to ")
    (pp lines))

  (var last-gb-index start-index)

  (var s (buffer/new 1))

  (var x 0)

  # position relative to local top
  # local top = y offset
  # if we scrolled 30px, and y offset is 5px
  # line-start-y is 35px, which means we skip lines above 35px
  (def line-start-y (+ (+ #(offset 1)
                          # we do max here to avoid lines popping in during animation
                          (- (max (gb :render-scroll) # (gb :scroll)
)))))

  # current line
  (var line-i nil)

  ### first skip all lines that are outside of the screen
  ### this would be lines scrolled path

  (loop [i :range [0 (length lines)]
         :let [line-y (y-poses i)]]
    (when (>= line-y (- line-start-y (* h 2))) # subtract h so partial lines will render
      (set last-gb-index (if (= i 0)
                           0
                           (lines i)))
      (set line-i i)
      # (print "skipped " i " lines")
      (break)))

  # just used for debugging
  (var nof-lines 0)

  (if (= line-i nil)
    :do-nothing
    (loop [i :range [line-i (length lines)]
           :let [l (lines i)
                 line-y (y-poses i)
                 target-y (rel-y gb (- line-y line-start-y))]
           :until (> target-y y-limit)]

      (++ nof-lines)
      (set x 0)

      (render-selection-box gb last-gb-index l (- line-y line-start-y))

      ### render line numbers
      (when (gb :show-line-numbers)
        (let [lns (string/format "%d" (line-numbers i))]
          (gb-draw-text gb
                        lns
                        [0
                         (rel-y gb (* y-scale (- line-y line-start-y)))]
                        :gray)))

      (gb-iterate
        gb
        last-gb-index l
        i c
        (let [[w h] (get-size sizes c)]

          (put s 0 c)

          (when debug
            (print "last: " last-gb-index " - l: " l)
            (prin s)
            (print (length s))
            #(print "x: " x " - y: " y)
)

          (do
            (while (and delim-ps
                        (< delim-i (length delim-ps))
                        (< (first (delim-ps delim-i)) i))
              (++ delim-i))

            (while (and highlighting
                        (< hl-i (length highlighting))
                        (< ((highlighting hl-i) 1) i))
              (++ hl-i)))

          (gb-draw-text gb
                        s
                        [(math/floor (rel-text-x gb x))
                         (math/floor (* y-scale target-y))]
                        (cond (in-selection? gb i)
                          :white

                          (and delim-ps
                               (< delim-i (length delim-ps))
                               (= ((delim-ps delim-i) 0) i))
                          (get rb/colors ((delim-ps delim-i) 1) :pink)

                          (and highlighting
                               (< hl-i (length highlighting))
                               (<= ((highlighting hl-i) 0) i))
                          (get colors ((highlighting hl-i) 2) default-text-color)

                          # else
                          (get gb :text/color
                               default-text-color)))

          (+= x (* x-scale w))))

      (when debug
        (print))

      (set last-gb-index l)))

  #  (print "rendered " nof-lines " lines")
)

(varfn line-of-i
  [gb i]
  (def {:lines lines
        :line-flags line-flags
        :stickiness stickiness} gb)

  (let [line-index (->> (binary-search-closest lines |(compare i $))
                        ## binary-search-closest returns length of 
                        ## array when outside length of array
                        (max 0)
                        (min (dec (length lines))))
        line-flag (line-flags line-index)]

    (if (and (= i (lines line-index)) ## end of line
             (= line-flag :word-wrap)
             (= stickiness :down))
      (inc line-index)
      line-index)))

(varfn current-line
  [gb]
  (line-of-i gb (gb :caret)))

(varfn current-line-number
  [gb]
  ((gb :line-numbers) (line-of-i gb (gb :caret))))

(varfn index-above-cursor-on-line
  [gb line]
  (let [{:lines lines
         :caret-pos caret-pos
         :memory-of-caret-x-pos mocxp} gb]
    (if (<= 0 line)
      (or (index-passing-max-width
            gb
            (get lines (dec line) 0) ## start of prev line
            (lines line) ## end of prev line
            mocxp) ## current x position
          (lines line))
      0)))

(varfn page-up!
  [gb]
  (if (zero? (gb :scroll))
    (-> gb
        deselect
        reset-blink
        (put-caret 0))
    (let [target (+ (- (gb :scroll))
                    (* 2 (gb :text/size)))
          line (dec (binary-search-closest (gb :y-poses) |(compare target $)))]
      (-> gb
          deselect
          reset-blink
          (put-caret (max 0 (index-above-cursor-on-line gb line)))
          (update :scroll + (- (height gb) (* 3 (gb :text/size))))
          (update :scroll min 0)
          (put :changed-scroll true)))))

(varfn page-down!
  [gb]
  (def lines (gb :lines))
  (def y-poses (gb :y-poses))
  (def line-flags (gb :line-flags))
  (def line-numbers (gb :line-numbers))

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (def target-pos (+ (height gb)
                     (- (gb :scroll))
                     (- (* 2 (gb :text/size)))))

  (let [line (binary-search-closest
               (gb :y-poses)
               |(compare target-pos $))]
    (if (= line (length (gb :lines)))
      (-> gb
          deselect
          reset-blink
          (put-caret (gb-length gb)))
      (-> gb
          deselect
          reset-blink
          (put-caret (index-above-cursor-on-line gb line))
          (update :scroll - (- (height gb) (* 3 (gb :text/size))))
          (update :scroll min 0)
          (put :changed-scroll true)))))

(varfn index-above-cursor
  [gb]
  (let [{:lines lines
         :caret-pos caret-pos
         :memory-of-caret-x-pos mocxp} gb
        prev-line (dec (current-line gb))]
    (if (<= 0 prev-line)
      (or (index-passing-max-width
            gb
            (get lines (dec prev-line) 0) ## start of prev line
            (lines prev-line) ## end of prev line
            mocxp) ## current x position
          (lines prev-line))
      0)))

(varfn index-below-cursor
  [gb]
  (let [{:lines lines
         :caret-pos caret-pos
         :memory-of-caret-x-pos mocxp} gb
        next-line (inc (current-line gb))]
    (if (< next-line (length lines))
      (or (index-passing-max-width
            gb
            (get lines (dec next-line) 0) ## start of next line
            (lines next-line) ## end of next line
            mocxp) ## current x position
          (lines next-line))
      (gb-length gb))))

(varfn index-start-of-line
  [gb]
  (let [{:lines lines
         :line-flags line-flags} gb
        prev-line (dec (current-line gb))]
    (if (<= 0 prev-line)
      (if (= :regular (line-flags prev-line))
        (inc (lines prev-line))
        (lines prev-line))
      0)))

(varfn move-to-start-of-line
  [gb]
  (-> gb
      deselect
      (put-caret (index-start-of-line gb))
      (put :stickiness :down)
      (put :changed-x-pos true)))

(varfn select-to-start-of-line
  [gb]
  (unless (gb :selection)
    (put gb :selection (gb :caret)))

  (-> gb
      (put-caret (index-start-of-line gb))
      (put :stickiness :down)
      (put :changed-selection true)
      (put :changed-x-pos true)))

(varfn move-to-end-of-line
  [gb]
  (-> gb
      deselect
      (put-caret ((gb :lines) (current-line gb)))
      (put :stickiness :right)
      (put :changed-x-pos true)))

(varfn select-to-end-of-line
  [gb]
  (unless (gb :selection)
    (put gb :selection (gb :caret)))

  (-> gb
      (put-caret ((gb :lines) (current-line gb)))
      (put :stickiness :right)
      (put :changed-selection true)
      (put :changed-x-pos true)))

(varfn focus-pos
  [gb pos]
  (-> gb
      (put :scroll (/ (-> (- (- (pos 1))
                             ((gb :offset) 1)
                             ((gb :position) 1)
                             (- (* 0.5 (- (min (get-screen-height)
                                               (height gb))
                                          ((gb :offset) 1)
                                          ((gb :position) 1)))))
                          (min 0))

                      1 #mult
))
      (put :changed-scroll true)))

(varfn focus-caret
  [gb]
  (let [[x y] (gb :caret-pos)
        y y # (+ y
        #  (- ((gb :y-poses) (line-of-i (gb :caret)))
        #     (get (gb :y-poses) (max (length (gb :y-poses))
        #                             (inc (line-of-i (gb :caret)))))))
]
    (focus-pos gb [x y])))

(varfn move-up!
  [gb]
  (-> gb
      deselect
      reset-blink
      (put-caret (index-above-cursor gb))))


(varfn line-number->line
  [gb line-number]

  (def lines (gb :lines))
  (def y-poses (gb :y-poses))
  (def line-flags (gb :line-flags))
  (def line-numbers (gb :line-numbers))

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (word-wrap-gap-buffer
    gb
    sizes
    lines
    y-poses
    line-flags
    line-numbers

    gb
    (or (last lines) 0)
    (if (empty? lines) nil (dec (length lines)))
    (gb-length gb)
    (inner-width gb)
    (* (gb :text/size) (gb :text/line-height))
    0
    99999999

    :line-limit line-number)

  (var line (binary-search-closest line-numbers |(compare line-number $)))

  (while (= line-number (get line-numbers (dec line)))
    (-- line))

  line)

(varfn goto-line-number
  [gb line-number]
  (let [line (line-number->line gb line-number)
        line (min (dec (length (gb :lines))) line)
        index (if (zero? line)
                0
                ((gb :lines) line))]
    (-> gb
        deselect
        reset-blink
        (put-caret index)
        move-to-start-of-line)))

(varfn select-move-up!
  [gb]
  (unless (gb :selection)
    (put gb :selection (gb :caret)))

  (-> gb
      (put-caret (index-above-cursor gb))
      (put :changed-selection true)))

(varfn i-at-beginning-of-line?
  [gb i]
  (let [l (dec (line-of-i gb i))
        lf (get-in gb [:line-flags l])]
    (= (if (= :word-wrap lf)
         i
         (dec i))
       (get-in gb [:lines l]))))

(varfn move-down!
  [gb]

  (deselect gb)

  ### this part is done to set stickiness to down
  # when the caret is at the far left of a line

  (if (i-at-beginning-of-line? gb (gb :caret))
    (put gb :stickiness :down)
    (put gb :stickiness :right))

  (reset-blink gb)

  (put-caret gb (index-below-cursor gb)))

(varfn select-move-down!
  [gb]
  (unless (gb :selection)
    (put gb :selection (gb :caret)))

  (-> gb
      (put-caret (index-below-cursor gb))
      (put :changed-selection true)))

(comment
  (index-above-cursor gb-data)
  (index-below-cursor gb-data)
  (move-up! gb-data)
  (move-down! gb-data))

(varfn index->pos!
  "Recalculates position and returns it."
  [gb index]

  (def lines (gb :lines))
  (def y-poses (gb :y-poses))
  (def line-flags (gb :line-flags))
  (def line-numbers (gb :line-numbers))

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (word-wrap-gap-buffer
    gb
    #(gb :sizes)
    sizes
    lines
    y-poses
    line-flags
    line-numbers

    gb
    (or (last lines) 0)
    (if (empty? lines) nil (dec (length lines)))
    index
    (inner-width gb)
    (* (gb :text/size) (gb :text/line-height))
    0
    99999999)

  (let [pos [(width-between
               gb
               (get lines (dec (line-of-i gb index)) 0)
               index)
             (last y-poses)]
        sizes (a/glyph-sizes (gb :text/font) (gb :text/size))]

    (word-wrap-gap-buffer
      gb
      #(gb :sizes)
      sizes
      lines
      y-poses
      line-flags
      line-numbers

      gb
      (or (last lines) 0)
      (if (empty? lines) nil (dec (length lines)))
      (gb-length gb)
      (inner-width gb)
      (* (gb :text/size) (gb :text/line-height))
      0
      (pos 1))

    [(width-between
       gb
       (get lines (dec (line-of-i gb index)) 0)
       index)

     (get y-poses (line-of-i gb index))]))

(comment
  (index->pos! gb-data 11000)

  (do (put gb-data :scroll
           (+ (- (last (index->pos! gb-data
                                    (gb-data :caret))))
              (* 0.25 (height gb-data))))
    (put gb-data :changed-selection true)
    :ok))

(varfn index->pos
  [gb index]
  (def {:lines lines
        :y-poses y-poses} gb)
  (let [# line-index (-> (max 0 (binary-search-closest
        #                         lines
        #                         |(compare index $)))
        #                (min (max 0 (dec (length lines)))))
        line-index (line-of-i gb index)
        x (width-between
            gb
            (get lines (dec line-index) 0)
            index)
        y (y-poses line-index)]

    (when (dyn :debug)
      (print "line-index: " line-index))

    [x y]))

(comment
  (gb-data :y-poses)

  (with-dyns [:debug true]
    (index->pos gb-data (gb-data :caret))))

(var timing-enabled false)
(set timing-enabled true)
(set timing-enabled false)

(defmacro timeit
  [label & body]
  ~(if timing-enabled
     (do
       (test/timeit (let [res (do ,;body)]
                      (print "timing: " ,label)
                      res)))
     (do ,;body)))

(varfn inner-draw
  [gb]
  (def {:position position
        :offset offset
        :size size
        :colors colors
        :scroll scroll
        :changed changed
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :line-flags line-flags}
    gb)

  (def [x-scale y-scale] screen-scale)

  (def [x y] position)
  (def [ox oy] offset)
  (def [w _] size)

  (def screen-w (* x-scale (get-screen-width)))
  (def screen-h (* y-scale (get-screen-height)))

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (render-lines sizes
                gb
                (gb :lines)
                0
                (* y-scale (* (gb :text/size) (gb :text/line-height)))
                (height gb)))

(varfnp generate-texture
        [gb]
        (begin-texture-mode (gb :texture))
        (rl-push-matrix)

        (rl-load-identity)

        # you can use blank for slightly thinner text
        (clear-background (or (when (not= (gb :scroll)
                                          (gb :render-scroll)) :blank)
                              (gb :background)
                              (get-in gb [:colors :background])
                              :blue))
        (inner-draw gb)
        (rl-pop-matrix)

        (end-texture-mode))

(defn document-bottom
  [gb y]
  (+ y
     (- (gb :scroll))
     (min (get-screen-height) (height gb))))

(defn ensure-render-texture
  [gb]
  (when-let [t (and (gb :resized)
                    (gb :texture))]
    (unload-render-texture t)
    (put gb :texture nil))

  (unless (gb :texture)
    (let [[x-scale y-scale] screen-scale
          w (* x-scale (width gb))
          h (* y-scale (height gb))
          rt (load-render-texture w h)]
      (put gb :texture rt))))

(defnp word-wrap
  [gb]
  (def {:position position
        :offset offset
        :size size
        :colors colors
        :scroll scroll
        :changed changed
        :lowest-changed-at change-pos
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :changed-styling changed-styling
        :resized resized
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :line-flags line-flags
        :line-numbers line-numbers}
    gb)

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (var start-line (cond (empty? lines)
                    nil

                    (not changed)
                    (dec (length lines))

                    (not change-pos)
                    nil

                    # else
                    (dec (line-of-i gb change-pos))))

  (while (= :word-wrap (get line-flags start-line))
    (-- start-line))

  (set start-line (if (neg? start-line) nil start-line))

  (def start-pos (if-not start-line
                   0
                   (lines start-line)))

  (word-wrap-gap-buffer
    gb
    sizes
    lines
    y-poses
    line-flags
    line-numbers

    gb
    start-pos
    start-line

    (gb-length gb)
    (inner-width gb)
    (* (gb :text/size) (gb :text/line-height))
    0
    (+ 0 (- (height gb) scroll))))

(defn refocus-caret
  [gb]
  (let [caret-y ((gb :caret-pos) 1)
        scroll (* 1 #mult
                  (- (gb :scroll)))]
    (when (< caret-y scroll)
      (focus-caret gb))

    # caret-y is relative to document
    (when (and (> caret-y 0)
               (>= caret-y
                   (document-bottom gb (- (* (gb :text/size) (gb :text/line-height))))))

      ## index->pos! recalculates lines etc
      ## in order to find the "real" position of the caret
      ## this function shouldn't be called needlessly
      (put gb :caret-pos (index->pos! gb (gb :caret)))
      (focus-caret gb))))

(varfn gb-pre-render
  [gb]
  (def {:position position
        :offset offset
        :size size
        :colors colors
        :scroll scroll
        :changed changed
        :lowest-changed-at change-pos
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :changed-scroll changed-scroll
        :changed-styling changed-styling
        :resized resized
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :line-flags line-flags
        :line-numbers line-numbers}
    gb)

  (unless (gb :render-scroll)
    (put gb :render-scroll (gb :scroll)))

  (def rs-changed (not= (gb :scroll) (gb :render-scroll)))

  (update gb :render-scroll
          (fn [rs]
            (let [scroll (gb :scroll)
                  org-diff (- scroll rs)
                  snap 1
                  diff (* org-diff 0.5)]
              (if (and (> diff (- snap))
                       (< diff snap))
                scroll
                (+ rs diff)))))

  (def [x y] size)
  (def [x-scale y-scale] screen-scale)

  (ensure-render-texture gb)

  (when (or resized
            changed
            changed-nav
            changed-selection
            changed-scroll
            changed-styling
            rs-changed)

    (when changed
      (put gb :delim-ps (rb/gb->delim-ps gb)))

    (when (or changed changed-scroll)
      (word-wrap gb))

    (put gb :caret-pos (index->pos gb (gb :caret)))

    (when changed-x-pos
      (put gb :memory-of-caret-x-pos (get-in gb [:caret-pos 0])))

    (when (and (not (gb :dont-refocus)) (or changed changed-nav))
      # can be changed by refocus-caret
      (put gb :changed-scroll nil)

      (refocus-caret gb))

    # refocusing can cause scroll to change
    # when that happens, we word wrap again,
    # since letters further down might be in view
    (when (gb :changed-scroll)
      (word-wrap gb))

    ### TODO: figure out where to refocus

    (when (or resized
              changed
              changed-selection
              changed-styling
              (get gb :changed-scroll changed-scroll)
              rs-changed)

      (put gb :width-of-last-line-number
           (if (gb :show-line-numbers)
             (first (gb-measure-text gb (string/format "%d" (length lines))))
             0))

      (generate-texture gb))

    #        (pp (ez-gb gb))

    (put gb :lowest-changed-at nil)
    (put gb :resized nil)
    (put gb :changed false)
    (put gb :changed-x-pos false)
    (put gb :changed-nav false)
    (put gb :changed-scroll false)
    (put gb :changed-selection false)))

(varfn gb-render-text
  [gb]
  (def {:position position
        :offset offset
        #        :sizes sizes
        :size size
        :colors colors
        :scroll scroll
        :changed changed
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :scroll scroll}
    gb)

  (def [x-scale y-scale] screen-scale)

  (def [x y] position)

  (def screen-w (* x-scale (get-screen-width)))
  (def screen-h (* y-scale (get-screen-height)))

  (rl-push-matrix)

  (def h (height gb))
  (def w (width gb))

  #  (rl-mult-matrixf-screen-scale)

  #€   (rl-load-identity)

  (def col (gb :text/color))
  (def text/size (gb :text/size))
  (def text/spacing (gb :text/spacing))

  (let [mag (- (gb :scroll) (gb :render-scroll))]
    (when (> (math/abs mag) 10)

      (put gb :text/color 0x00ff00ff)

      #(rl-translatef (* 0.003 (math/abs mag) (dec (* 2 (math/random))))
      #               (* 0 mag (math/random)) 0)

      (rl-push-matrix)

      #(rl-scalef 0 #(inc (* -0.0001 (math/abs mag))
      #           (inc (* -0.0001 (math/abs mag))) 0)

      (def smear-y (- (min 25 (max -25 (* 0.05 mag)))))

      (rl-translatef 0 smear-y 0)

      (do #comment

        (def stretch-y (inc (* 0.002 (math/abs mag))))

        (def y-diff
          (if (neg? mag)
            (- (* h stretch-y)
               h)
            0))

        (draw-texture-pro
          (get-render-texture (gb :texture))
          [0 0 (* x-scale w)
           (* y-scale (- h)) # (- h) #screen-w (- screen-h)
]

          [x
           (- y (* 0.7 y-diff))
           w (* h stretch-y) #(/ screen-w x-scale) (/ screen-h y-scale)
]
          [0 0]
          0
          0x999999955
          #(colors :background)
)

        (draw-texture-pro
          (get-render-texture (gb :texture))
          [0 0 (* x-scale w)
           (* y-scale (- h)) # (- h) #screen-w (- screen-h)
]

          [x
           (- y (* 1.1 y-diff))
           w (* h stretch-y) #(/ screen-w x-scale) (/ screen-h y-scale)
]
          [0 0]
          0
          0x99999922
          #(colors :background)
))

      (rl-pop-matrix))

    ### CLEAR BACKGROUND
    #(clear-background (or (gb :background)
    #                      (colors :background)
    #                      :blue))

    (put gb :text/color col)
    (put gb :text/size text/size)
    (put gb :text/spacing text/spacing)

    #  (inner-draw gb)

    (do #comment
      (def ratio (- 1 (math/abs (* mag 0.00005))))
      (def extra-y (if (pos? mag)
                     0
                     (if (not= 0 mag)
                       (* (- 1 ratio) h)
                       (* (- 1 ratio) h))))

      (draw-texture-pro
        (get-render-texture (gb :texture))
        [0 0 (* x-scale w)
         (* y-scale (- (* ratio h))) # (- h) #screen-w (- screen-h)
]

        [x
         (+ y extra-y)
         w (* ratio h) #(/ screen-w x-scale) (/ screen-h y-scale)
]
        [0 0]
        0
        :white
        #(colors :background)
))
    #
)

  (rl-pop-matrix)

  (when timing-enabled
    (print)
    (print)
    (set timing-enabled false)))

(varfn render-cursor
  [gb]
  (def {:position position
        :offset offset
        #        :sizes sizes
        :size size
        :colors colors
        :scroll scroll
        :changed changed
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :scroll scroll} gb)
  (rl-push-matrix)

  #(rl-load-identity)

  #  (rl-scalef 2 2 1)

  (when :caret-pos

    (unless (gb :render-caret-pos)
      (put gb :render-caret-pos @[;(gb :caret-pos)]))

    (let [[x y] (gb :render-caret-pos)
          [tx ty] (gb :caret-pos)
          org-diff-x (- tx x)
          diff-x (* org-diff-x 0.7)

          org-diff-y (- ty y)
          diff-y (* org-diff-y 0.7)

          new-x (+ x diff-x)
          new-y (+ y diff-y)]
      (put-in gb [:render-caret-pos 0] new-x)
      (put-in gb [:render-caret-pos 1] new-y)

      (let [[x y] (gb :render-caret-pos)
            cx (abs-text-x gb x)
            cy (abs-text-y gb (+ y scroll))
            extra-x (min 10 (max -10
                                 (*
                                   org-diff-x
                                   -0.5)))
            extra-y (* org-diff-y 0.5)]

        (put gb :dbg-y2 scroll)
        (put gb :dbg-y1 y)
        (put gb :dbg-y (abs-text-y gb (+ y scroll)))

        (draw-line-ex
          [(+ cx extra-x) cy]
          [(+ cx (* 1.5 extra-x))
           (+ cy
              (* 1 (math/abs extra-y))
              (- (* (gb :text/size)
                    (gb :text/line-height))
                 1))]
          (min 3 (inc (if (zero? extra-y)
                        (math/abs extra-x)
                        (math/abs extra-y))))
          (or 0x999999ff (gb :caret/color) (get-in gb [:colors :caret])))

        (draw-line-ex
          [(+ cx extra-x) cy]
          [(+ cx (* extra-x 1.5))
           (+ cy
              (- (* (gb :text/size)
                    (gb :text/line-height))
                 1))]

          1
          (or (gb :caret/color) (get-in gb [:colors :caret]))))

      (rl-pop-matrix))))

(comment

  (var last-i 0)
  (loop [l :in lines]
    (gb-iterate gb
                last-i l
                i c
                (prin (-> (buffer/new 1)
                          (buffer/push-byte c))))
    (print)
    (set last-i l)))
