(import spork/test)
(use jaylib)
(import ./dumb :prefix "")
(import ./new_gap_buffer :prefix "")
(import ./textfield_api :prefix "")
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")
(import ./rainbow :as rb :fresh true)
(import ./highlighting :as hl :fresh true)

(def font-h 19)

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
        sz)
      sz)))

(varfn index-passing-max-width
  "Returns the index of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [gb start stop max-width]

  (def {:sizes sizes} gb)

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

  (def {:sizes sizes} gb)

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
  (with-dyns [:debug true]
    (let [gb {:text @"abc"
              :sizes (gb-data :sizes)
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
   stop
   width
   h
   y-offset
   y-limit]

  (var x 0)
  (var y 0)
  (var w 0)
  (var line-number 1)

  (var s (buffer/new 1))
  (var beginning-of-word-i 0)

  (def {:offset offset
        :width-of-last-line-number width-of-last-line-number} gb)
  (def [x-scale _] screen-scale)

  (array/clear lines)
  (array/clear y-poses)
  (array/clear line-flags)
  (array/clear line-numbers)

  (def treshold (- width (offset 0) (or width-of-last-line-number 0)))

  (defn check-if-word-is-too-wide
    [old-w i c]
    (if (> (+ x old-w) treshold) ## we went outside the max width
      (do ## so rowbreak before the word
        (when (not= beginning-of-word-i 0)
          (array/push lines beginning-of-word-i)
          (array/push line-numbers line-number)
          (array/push y-poses y)
          (array/push line-flags :wordwrap)
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
            (array/push line-flags :wordwrap)
            (+= y h))))

      (+= x (+ old-w (first (get-size sizes c))))))

  (gb-iterate
    gb
    start stop
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
  (def {:sizes sizes} gb)
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
        :conf conf
        :caret caret
        :sizes sizes
        :offset offset
        :width-of-last-line-number width-of-last-line-number} gb)
  (def [x-scale y-scale] screen-scale)

  (when selection
    (let [sel-start (min selection caret)
          sel-stop (max selection caret)]

      (when (and (< sel-start stop)
                 (>= sel-stop start))

        (var line-h font-h)
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

(varfn render-lines
  "Renders the lines in gap buffer.
Also renders selection boxes.
Render lines doesn't modify anything in gb."
  [sizes conf gb lines start-index unused-start-y h y-limit]
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
      (break)))

  (if (= line-i nil)
    :do-nothing
    (loop [i :range [line-i (length lines)]
           :let [l (lines i)
                 line-y (y-poses i)]]
      (set x 0)

      (render-selection-box gb last-gb-index l (- line-y line-start-y))

      ### render line numbers
      (when (gb :show-line-numbers)
        (let [lns (string/format "%d" (line-numbers i))
              lns-offset (defn measure-text
                           [tc text]
                           (measure-text-ex (tc :font)
                                            text
                                            (math/floor (* (tc :mult) (tc :size)))
                                            (* (tc :mult) (tc :spacing))))]
          (draw-text*2 conf
                       lns
                       [0
                        (rel-y gb (- line-y line-start-y))]
                       :gray
                       x-scale)))

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

          (draw-text*2 conf
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
                              default-text-color))
                       x-scale)

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
             (= line-flag :wordwrap)
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

                      (get-in gb [:conf :mult])))
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
    (= (if (= :wordwrap lf)
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

  (word-wrap-gap-buffer
    gb
    (gb :sizes)
    lines
    y-poses
    line-flags
    line-numbers

    gb
    0 index
    (inner-width gb)
    font-h
    0
    99999999)

  (let [pos [(width-between
               gb
               (get lines (dec (line-of-i gb index)) 0)
               index)
             (last y-poses)]]
    (word-wrap-gap-buffer
      gb
      (gb :sizes)
      lines
      y-poses
      line-flags
      line-numbers

      gb
      0 (gb-length gb)
      (inner-width gb)
      font-h
      0
      pos)

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
        :y-poses y-poses
        :conf conf} gb)
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
  (gb-data :conf)
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
        :sizes sizes
        :size size
        :conf conf
        :colors colors
        :scroll scroll
        :changed changed
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :width-of-last-line-number width-of-last-line-number
        :lines lines
        :y-poses y-poses
        :line-flags line-flags
        :scroll scroll}
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

  (clear-background (or (gb :background)
                        (colors :background)
                        #:blue
)
                    #(colors :background)
)

  (timeit "render lines"
          (render-lines sizes
                        conf
                        gb
                        (gb :lines)
                        0
                        (+ (offset 1) (- (gb :scroll)))
                        (* y-scale font-h)
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

(varfn gb-pre-render
  [gb]
  (def {:position position
        :offset offset
        :sizes sizes
        :size size
        :conf conf
        :colors colors
        :scroll scroll
        :changed changed
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
        :line-numbers line-numbers
        :scroll scroll}
    gb)
  (def [x-scale y-scale] screen-scale)

  (def [x y] position)
  (def [w _] size)

  (def screen-w (* x-scale (get-screen-width)))
  (def screen-h (* y-scale (get-screen-height)))

  (when-let [t (and (gb :resized)
                    (gb :texture))]
    (unload-render-texture t)
    (put gb :texture nil))

  (when (not (gb :texture))
    (put gb :texture (load-render-texture (* x-scale (width gb))
                                          (* y-scale (height gb)) #screen-w screen-h
)))

  (when (or resized
            changed
            changed-nav
            changed-selection
            changed-scroll
            changed-styling)

    (when changed
      (set timing-enabled false) # set-timing

      (timeit "delim ps"
              (put gb :delim-ps (rb/gb->delim-ps gb))))

    (when changed
      #      (timeit "hl" (put gb :highlighting (hl/gb->styling gb)))
)

    (def lines (or lines @[]))
    (put gb :lines lines)

    (def y-poses (or y-poses @[]))
    (put gb :y-poses y-poses)

    (def line-flags (or line-flags @[]))
    (put gb :line-flags line-flags)

    (def line-numbers (or line-numbers @[]))
    (put gb :line-numbers line-numbers)

    (var lines (if (or changed changed-scroll)
                 (timeit "word wrap" (let [sizes sizes]
                                       (word-wrap-gap-buffer
                                         gb
                                         sizes
                                         lines
                                         y-poses
                                         line-flags
                                         line-numbers

                                         gb
                                         0
                                         (gb-length gb)
                                         (inner-width gb)
                                         font-h
                                         y
                                         (+ y (- (height gb) scroll)))))
                 lines))

    (put gb :width-of-last-line-number
         (* x-scale
            (first (measure-text conf (string/format "%d" (length lines))))))

    (put gb :caret-pos (index->pos gb (gb :caret)))

    (when changed-x-pos
      (put gb :memory-of-caret-x-pos (get-in gb [:caret-pos 0])))

    (when (and (not (gb :dont-refocus)) (or changed changed-nav))
      (timeit "carety things"
              (let [caret-y ((gb :caret-pos) 1)
                    scroll (* (conf :mult) (- (gb :scroll)))]
                (when (< caret-y scroll)
                  (focus-caret gb))

                # caret-y is relative to document
                (when (and (> caret-y 0)
                           (>= caret-y
                               (document-bottom gb (- font-h))))

                  ## index->pos! recalculates lines etc
                  ## in order to find the "real" position of the caret
                  ## this function shouldn't be called needlessly
                  (put gb :caret-pos (index->pos! gb (gb :caret)))
                  (focus-caret gb)))))

    (when (or resized
              changed
              changed-selection
              changed-styling
              (gb :changed-scroll))
      (timeit "generate texture" (generate-texture gb)))

    #        (pp (ez-gb gb))

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
        :sizes sizes
        :size size
        :conf conf
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
    :white)

  (rl-pop-matrix)

  (when timing-enabled
    (print)
    (print)
    (set timing-enabled false)))

(varfn render-cursor
  [gb]
  (def {:position position
        :offset offset
        :sizes sizes
        :size size
        :conf conf
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
      [cx (+ cy (- font-h 1))]
      1
      (or (gb :caret/color) (get-in gb [:colors :caret]))))

  (rl-pop-matrix))

(comment
  (do (def gb {:text @"a b c "
               :gap-start 0
               :gap-stop 0
               :gap @"1 2 3 "})
    (def lines (let [sizes (text-data :sizes)
                     lines @[]]
                 (word-wrap-gap-buffer
                   sizes
                   lines
                   gb
                   0 (gb-length gb)
                   30
                   font-h
                   0
                   1000)))

    (var last-i 0)
    (loop [l :in lines]
      (gb-iterate gb
                  last-i l
                  i c
                  (prin (-> (buffer/new 1)
                            (buffer/push-byte c))))
      (print)
      (set last-i l))))
