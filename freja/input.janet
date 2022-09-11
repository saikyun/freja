(use freja-jaylib)
(import freja/state)
(import ./dumb :prefix "")
(import ./new_gap_buffer :as gb)
(import ./render_new_gap_buffer :as render-gb)
(import ./code_api :prefix "")
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")
(import ./collision :prefix "")

(def mouse-events {:mouse/down :mouse/press
                   :mouse/move :mouse/move
                   :mouse/drag :mouse/drag
                   :mouse/release :mouse/release
                   :mouse/double-click :mouse/double-click
                   :mouse/triple-click :mouse/triple-click
                   :mouse/scroll :mouse/scroll})

(defn new-mouse-data
  []
  @{:just-down nil
    :just-double-clicked nil
    :just-triple-clicked nil
    :recently-double-clicked nil
    :recently-triple-clicked nil
    :down-pos nil
    :down-time nil
    :down-time2 nil
    :up-pos nil
    :selected-pos nil
    :last-text-pos nil})

(defn key-down?
  [k]
  (state/keys-down k))

(defn meta-down?
  []
  (if (= :macos (os/which))
    (or (key-down? :left-super)
        (key-down? :right-super))
    (or (key-down? :left-control)
        (key-down? :right-control))))

## delay before first repetition of held keys
# TODO: if these delays are set to super low, frp bugs and wont release keys
(var initial-delay 0.2)

## delay of each repetition thereafter
(var repeat-delay 0.03)

## stores held keys and the delay until they should trigger
(var delay-left @{})

(def modifiers [:caps-lock :control :right-control :alt :shift
                :right-alt #:meta
])

(def check-modifiers
  {:caps-lock |(and (not= $ :caps-lock)
                    (key-down? :caps-lock))
   #:control |(and (not= $ :left-control)
   #               (not= $ :right-control)
   #               (or (key-down? :left-control)
   #                   (key-down? :right-control)))

   :control |(and (not= $ :left-control)
                  (key-down? :left-control))
   :right-control |(and (not= $ :right-control)
                        (key-down? :right-control))

   :shift |(and (not= $ :left-shift)
                (not= $ :right-shift)
                (or (key-down? :left-shift)
                    (key-down? :right-shift)))
   # :meta meta-down?
   :alt |(and (not= $ :left-alt)
              (key-down? :left-alt))
   :right-alt |(and (not= $ :right-alt)
                    (key-down? :right-alt))})

(defn set-key
  [kmap ks f]
  (var key nil)
  (var mods @[])

  # this ensures mods are ordered the same way
  (loop [m :in modifiers
         k :in ks]
    (if (= k m)
      (array/push mods k)
      (set key k)))

  (array/push mods key)
  (put-in kmap mods f))

(defn hotkey-triggered
  [kmap key-code kind]

  (def mods (seq [m :in modifiers
                  :when ((in check-modifiers m) key-code)]
              m))

  (var ret-f nil)
  (loop [[k f] :pairs (or (get-in kmap mods) [])
         :when (= k key-code)]
    (if-let [specific-f (and (table? f)
                             (in f kind))]
      (do
        (set ret-f specific-f)
        (break))

      (when (and (or (= kind :key-down)
                     (= kind :key-repeat)
                     (= kind :char))
                 (function? f))
        (set ret-f f)
        (break))))

  (if ret-f
    ret-f
    (when-let [p (and (not ret-f)
                      (table/getproto kmap))]
      (hotkey-triggered p key-code kind))))

(defn get-hotkey
  [kmap f &opt keys]
  (default keys [])

  (var ret-ks nil)

  (loop [[k f2] :pairs kmap]
    (cond (and (function? f2)
               (= f f2))
      (do (set ret-ks [;keys k])
        (break))

      (or (table? f2) (struct? f2))
      (set ret-ks (get-hotkey f2 f [;keys k]))
      (when ret-ks (break))))

  (if ret-ks ret-ks
    (when-let [p (table/getproto kmap)]
      (get-hotkey p f keys))))

(comment
  (= global-keys (table/getproto (gb-data :binds)))
  (get-hotkey (gb-data :binds) redo!)
  (get-hotkey global-keys redo!)
  (get-hotkey global-keys gb/copy)
  #
)


(defn handle-keyboard2
  [props k kind]
  (def {:binds binds} props)

  # TODO: Need to add get-char-pressed

  (when-let [f (hotkey-triggered (props :binds) k kind)]
    (f props))

  (comment
    (when-let [f (binds k)]
      (reset-blink props)
      (f props)

      (scroll-to-focus props))))

(defn handle-keyboard-char
  [props k]

  (def {:binds binds} props)

  # TODO: Need to add get-char-pressed

  (reset-blink props)

  (let [f (hotkey-triggered (props :binds) k :char)]
    (cond
      f (f props)

      (or (key-down? :left-shift)
          (key-down? :right-shift))
      (gb/insert-char-upper! props k)

      (gb/insert-char! props k))

    (scroll-to-focus props)))

(defn handle-scroll-event
  [props move]
  (if-let [max-y (-?> (props :y-poses)
                      last
                      -
                      (* (screen-scale 1)))]
    (update props :scroll |(max max-y (min 0 (+ $ (* move 2)))))
    (update props :scroll |(min 0 (+ $ (* move 2)))))

  (put props :changed-scroll true))

(defn get-mouse-pos
  [props [mx my]]

  (def {:lines lines
        :y-poses y-poses
        :line-flags line-flags
        :position position
        :offset offset
        :width-of-last-line-number width-of-last-line-number
        :scroll scroll} props)

  (def [x-pos y-pos] position)
  (def [ox oy] offset)

  (def y-offset (+ oy y-pos (* # (conf :mult)
                               scroll)))

  (def [x-scale _] screen-scale)

  (unless (empty? lines)
    (let [line-index (-> (binary-search-closest
                           y-poses
                           |(compare my (+ $ y-offset)))
                         dec
                         (max 0))
          row-start-pos (if (= 0 line-index)
                          0
                          (lines (dec line-index)))
          row-end-pos (lines line-index)
          char-i (render-gb/index-passing-middle-max-width
                   props
                   row-start-pos
                   row-end-pos
                   # take mouse x in absolute space
                   # and remove the stuff left of
                   # the beginning of the row (e.g. line number width)
                   (- mx
                      (render-gb/abs-text-x props 0)))

          flag (line-flags (max 0 (dec line-index)))]

      (min
        (gb/gb-length props)
        (cond
          (zero? char-i) char-i

          (and (= flag :regular)
               (= row-start-pos char-i)) ## to the left of \n
          (inc char-i)

          char-i)))))

(defn get-mouse-pos-line
  [props [mx my]]

  (def {:lines lines
        :y-poses y-poses
        :line-flags line-flags
        :position position
        :offset offset
        :width-of-last-line-number width-of-last-line-number
        :scroll scroll} props)

  (def [x-pos y-pos] position)
  (def [ox oy] offset)

  (def y-offset (+ oy y-pos (* # (conf :mult)
                               scroll)))

  (def [x-scale _] screen-scale)

  (unless (empty? lines)
    (let [line-index (-> (binary-search-closest
                           y-poses
                           |(compare my (+ $ y-offset)))
                         dec
                         (max 0))
          row-start-pos (if (= 0 line-index)
                          0
                          (lines (dec line-index)))
          row-end-pos (lines line-index)
          char-i (render-gb/index-passing-middle-max-width
                   props
                   row-start-pos
                   row-end-pos
                   # take mouse x in absolute space
                   # and remove the stuff left of
                   # the beginning of the row (e.g. line number width)
                   (- mx
                      (render-gb/abs-text-x props 0)))

          flag (line-flags (max 0 (dec line-index)))]

      [(max 0 (dec line-index))
       (min
         (gb/gb-length props)
         (cond
           (zero? char-i) char-i

           (and (= flag :regular)
                (= row-start-pos char-i)) ## to the left of \n
           (inc char-i)

           char-i))])))


(defn handle-shift-mouse-down
  [props [kind mouse-pos] cb]
  (def {:lines lines
        :offset offset
        :position position
        :y-poses y-poses
        :sizes sizes
        :scroll scroll} props)

  (def [ox oy] offset)
  (def [x-pos y-pos] position)

  (def [x y] mouse-pos)

  (def y-offset (+ y-pos oy (* #(conf :mult)
                               scroll)))
  (def x-offset (+ x-pos ox))

  (if (nil? (props :selection))
    (cb kind |(-> props
                  (put :selection (props :caret))
                  (gb/put-caret (get-mouse-pos props mouse-pos))
                  (put :stickiness (if (< x x-offset) :down :right))
                  (put :changed-selection true)))

    (cb kind |(let [curr-pos (get-mouse-pos props mouse-pos)
                    start (min (props :selection) (props :caret))
                    stop (max (props :selection) (props :caret))]
                (-> props
                    (put :selection
                         (if (> curr-pos start)
                           start
                           stop))
                    (gb/put-caret curr-pos)
                    (put :stickiness (if (< x x-offset) :down :right))
                    (put :changed-selection true))))))

(defn gb-rec
  [{:offset offset
    :position position
    :size size
    :scroll scroll}]

  (def [ox oy] offset)
  (def [x-pos y-pos] position)

  (def y-offset 0) # (+ y-pos oy)) # (* (conf :mult) scroll)))
  (def x-offset 0) #(+ x-pos ox))

  [x-offset
   y-offset
   (size 0)
   (size 1)])

(defn handle-mouse-event
  [props event cb]
  (def {:lines lines
        :offset offset
        :position position
        :y-poses y-poses
        :size size
        :sizes sizes
        :scroll scroll
        :width-of-last-line-number width-of-last-line-number} props)
  (def {:mouse/pos mouse-pos} event)
  (def [x y] mouse-pos)

  (def [ox oy] offset)
  (def [x-pos y-pos] position)

  (def y-offset (+ y-pos oy (* 1 #mult
                               scroll)))
  (def x-offset (+ x-pos ox))

  (when (in-rec? mouse-pos
                 (gb-rec props))
    (match event
      {:mouse/release _}
      (cb |(put props :down-index nil))

      {:mouse/triple-click _}
      (cb |(gb/select-region props ;(gb/find-surrounding-paragraph!
                                      props
                                      (get-mouse-pos props mouse-pos))))
      #                       should maybe remember original pos

      ({:mouse/double-click _}
        (and (not (key-down? :left-shift))
             (not (key-down? :right-shift))))
      (cb |(gb/select-region props ;(gb/word-at-index props
                                                      (get-mouse-pos props mouse-pos))))
      #                                    should maybe remember original pos

      ({:mouse/down _}
        (and (or (key-down? :left-shift)
                 (key-down? :right-shift))))
      (handle-shift-mouse-down props event cb)

      {:mouse/down _}
      (if-let [f (get-in props [:binds :press])]
        (cb |(f (get-mouse-pos-line props mouse-pos)))
        (cb |(let [[line cur-index] (get-mouse-pos-line props mouse-pos)
                   #              # if true, this means cur-index is at the start of the line
                   stickiness (if (= cur-index (lines line))
                                :down
                                :right)]
               (-> props
                   reset-blink
                   (put :down-index cur-index)
                   (put :selection nil)
                   (put :changed-selection true)
                   (put :caret cur-index)
                   (put :changed-x-pos true)
                   (put :stickiness stickiness)
                   (put :changed-nav true)))))

      ({:mouse/drag _}
        # if this is nil, the press happened outside the textarea
        (props :down-index))
      (cb |(let [down-pos (props :down-index)
                 curr-pos (get-mouse-pos props mouse-pos)]

             (if (not= down-pos curr-pos)
               (-> props
                   (put :selection down-pos)
                   (put :changed-selection true))
               (-> props
                   (put :selection nil)
                   (put :changed-selection true)))

             (-> props
                 (put :caret curr-pos)
                 (put :stickiness (if (< x x-offset) :down :right))
                 (put :changed-nav true)))))))

# table containing all positions that should be
# offset by `offset-event-pos`
(def event-positions
  @{:mouse/down :mouse/press
    :mouse/drag :mouse/drag
    :mouse/move :mouse/move
    :mouse/release :mouse/release
    :mouse/double-click :mouse/double-click
    :mouse/triple-click :mouse/triple-click
    :mouse/pos :mouse/pos})

(defn offset-event-pos
  ``
  Offsets all (known) positions in an event.
  This is used to transform an event from global positions,
  to local positions, e.g. the top left of a textarea being [0 0].
  ``
  [ev ox oy &keys {:scale scale}]
  (default scale 1)

  (let [new-ev (table/clone ev)]
    (loop [[k v] :pairs new-ev
           :when (event-positions k)]
      (let [[x y] v]
        (put new-ev k [(math/floor (/ (- x ox) scale))
                       (math/floor (/ (- y oy) scale))])))
    new-ev))
