(import spork/test)
(use freja-jaylib)
(import ./dumb :prefix "")
(import ./new_gap_buffer :prefix "")
(import ./textfield_api :prefix "")
(import ./text_rendering :as tr)
(import ./find_row_etc :prefix "")
(import ./assets :as a)
(import ./rainbow :as rb :fresh true)
(import ./highlighting :as hl :fresh true)

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

(varfn get-size
  [sizes c]
  (let [sz (get sizes c)]
    (if-not sz
      (let [sz (first (values sizes))]
        (print "no size for char " c ", using first sizes instead." sz)
        (debug/stacktrace (fiber/current))
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
   y-limit]

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
    (tracev start)
    (tracev stop)
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

    (when (> (+ y y-offset) y-limit)
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

  (tracev lines)

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
             (+ (- y (* y-scale 0)) (offset 1))
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
  (+ ((gb :position) 0)
     ((gb :offset) 0)
     (gb :width-of-last-line-number)
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

(import freja/fonts :as f)

(defn gb-draw-text
  [gb text pos color]
  (def font (a/font (gb :text/font) (gb :text/size)))
  (tr/draw-text* font
                 text
                 pos
                 (gb :text/size)
                 (gb :text/spacing)
                 color))

(defn gb-measure-text
  [gb text]
  (def font (a/font (gb :text/font) (gb :text/size)))
  (tr/measure-text* font
                    text
                    (gb :text/size)
                    (gb :text/spacing)))


(varfn render-lines
  "Renders the lines in gap buffer.
Also renders selection boxes.
Render lines doesn't modify anything in gb."
  [sizes gb lines start-index unused-start-y h y-limit]
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
                          (- (gb :scroll)))))

  # current line
  (var line-i nil)

  ### first skip all lines that are outside of the screen
  ### this would be lines scrolled path

  (loop [i :range [0 (length lines)]
         :let [line-y (y-poses i)]]
    (when (>= line-y (- line-start-y h)) # subtract h so partial lines will render
      (set last-gb-index (if (= i 0)
                           0
                           (lines i)))
      (set line-i i)
      (print "skipped " i " lines")
      (break)))

  (when line-i
    (print "rendering " (- (length lines) line-i) " lines"))

  (if (= line-i nil)
    :do-nothing
    (loop [i :range [line-i (length lines)]
           :let [l (lines i)
                 line-y (y-poses i)]]
      (set x 0)

      (render-selection-box gb last-gb-index l (- line-y line-start-y))

      ### render line numbers
      (when (gb :show-line-numbers)
        (let [lns (string/format "%d" (line-numbers i))]
          (gb-draw-text gb
                        lns
                        [0
                         (rel-y gb (- line-y line-start-y))]
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
                         (math/floor (rel-y gb (- line-y line-start-y)))]
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

      (set last-gb-index l))))

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

(comment
  (current-line gb-data))

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
  (print "focus caret")
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
  "Recalculates position and returns it.
This function is pretty expensive since it redoes all word wrapping."

  #### tldr this function is ridiculous

  # a possible improvement would be to store the min / max indexes
  # used during the last word wrap calculation
  # then if the index is outside that, redo the calculation
  # another alternative would be to generally to rendering
  # relative to indexes rather than having a global scroll which
  # is dependent on all lines before it
  [gb index]

  (def lines (gb :lines))
  (def y-poses (gb :y-poses))
  (def line-flags (gb :line-flags))
  (def line-numbers (gb :line-numbers))

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (print "index->pos first")
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

    (print "index->pos second")
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

(varfn generate-texture
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
        :line-flags line-flags}
    gb)

  (def [x-scale y-scale] screen-scale)

  (def [x y] position)
  (def [ox oy] offset)
  (def [w _] size)

  (def screen-w (* x-scale (get-screen-width)))
  (def screen-h (* y-scale (get-screen-height)))

  (begin-texture-mode (gb :texture))

  (rl-push-matrix)

  (rl-load-identity)

  #  (rl-scalef 1 1 1)

  # you can use blank for slightly thinner text
  (clear-background (or #:blank
                        (gb :background)
                        (colors :background)
                        :blue))

  (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

  (timeit "render lines"
          (render-lines sizes
                        gb
                        (gb :lines)
                        0
                        (+ (offset 1) (- (gb :scroll)))
                        (* y-scale (* (gb :text/size) (gb :text/line-height)))
                        (height gb)))

  # not sure why I have to do this 
  # I thought rl-pop-matrix would be enough
  #  (rl-load-identity) 

  #  (rl-scalef 2 2 1)

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

(varfn gb-pre-render
  [gb]
  (def {:position position
        :offset offset
        #        :sizes sizes
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

  (def [x y] size)
  (def [x-scale y-scale] screen-scale)

  (ensure-render-texture gb)

  (when (or resized
            changed
            changed-nav
            changed-selection
            changed-scroll
            changed-styling)
    (p :when-changed
       (do
         (when changed
           (put gb :delim-ps (rb/gb->delim-ps gb)))

         (def sizes (a/glyph-sizes (gb :text/font) (gb :text/size)))

         (var start-line (cond (empty? lines)
                           nil

                           (not changed)
                           (dec (length lines))

                           (not change-pos)
                           nil

                           # else
                           (dec (line-of-i gb change-pos))))

         (when (not changed)
           (while (= :word-wrap (get line-flags start-line))
             (-- start-line)))

         (set start-line (if (neg? start-line) nil start-line))

         (def start-pos (if-not start-line
                          0
                          (lines start-line)))

         (var lines (if (or changed (tracev changed-scroll))
                      (do
                        (print "calc word wrap again")
                        (p :word-wrap (let [sizes sizes]
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
                                          y
                                          (+ y (- (height gb) scroll))))))
                      lines))

         (put gb :width-of-last-line-number
              (* x-scale
                 (first (gb-measure-text gb (string/format "%d" (length lines))))
                 #
))

         (put gb :caret-pos (index->pos gb (gb :caret)))

         (when changed-x-pos
           (put gb :memory-of-caret-x-pos (get-in gb [:caret-pos 0])))

         (when (and (not (gb :dont-refocus)) (or changed changed-nav))
           (p :caret-thing
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
                  (focus-caret gb)))))

         ### TODO: figure out where to refocus

         (when (or resized
                   changed
                   changed-selection
                   changed-styling
                   (gb :changed-scroll))
           (p :gen-texture (generate-texture gb)))

         #        (pp (ez-gb gb))

         (put gb :lowest-changed-at nil)
         (put gb :resized nil)
         (put gb :changed false)
         (put gb :changed-x-pos false)
         (put gb :changed-nav false)
         (put gb :changed-scroll false)
         (put gb :changed-selection false)))))

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

  #  (rl-scalef 2 2 1)

  ### CLEAR BACKGROUND
  #(clear-background (or (gb :background)
  #                      (colors :background)
  #                      :blue))
  (draw-texture-pro
    (get-render-texture (gb :texture))
    [0 0 (* x-scale w)
     (* y-scale (- h)) # (- h) #screen-w (- screen-h)
]

    [x
     y
     w h #(/ screen-w x-scale) (/ screen-h y-scale)
]
    [0 0]
    0
    :white
    #(colors :background)
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

  (when-let [[x y] (gb :caret-pos)
             cx (abs-text-x gb x)
             cy (abs-text-y gb (+ y scroll))]

    (put gb :dbg-y2 scroll)
    (put gb :dbg-y1 y)
    (put gb :dbg-y (abs-text-y gb (+ y scroll)))

    (draw-line-ex
      [cx cy]
      [cx (+ cy (- (* (gb :text/size) (gb :text/line-height)) 1))]
      1
      (or (gb :caret/color) (get-in gb [:colors :caret]))))

  (rl-pop-matrix))

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
