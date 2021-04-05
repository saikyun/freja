(use jaylib)
(import ./eval :prefix "")
(import ./new_gap_buffer :prefix "")
(import ./render_new_gap_buffer :prefix "")
(import ./file_handling :prefix "")
(import ./code_api :prefix "")
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")

(varfn new-mouse-data
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

(varfn eval-it
  [data code]
  (print "Eval! " code)
  (try (do (fiber/setenv (fiber/current) (data :top-env))
         (def res (eval-string code))
         (put data :latest-res (string/format "%.4m" res)))
    ([err fib]
      (put data :latest-res err)))
  (pp (data :latest-res)))

(varfn eval-it2
  [env code]
  (print "Eval! " code)
  (try (do (fiber/setenv (fiber/current) env)
         (def res (eval-string code))
         (pp res))
    ([err fib]
      (debug/stacktrace fib err))))

(varfn meta-down?
  []
  (if (= :macos (os/which))
    (or (key-down? :left-super)
        (key-down? :right-super))
    (or (key-down? :left-control)
        (key-down? :right-control))))

## stores held keys and the delay until they should trigger
(var delay-left @{})

(varfn focus
  [{:id id :context context}]
  (set delay-left @{})
  (put context :focus id))

(varfn focus-other
  [{:context context} id]
  (set delay-left @{})
  (put context :focus id))

## delay before first repetition of held keys
(var initial-delay 0.2)

## delay of each repetition thereafter
(var repeat-delay 0.03)

(def kw->f {:paste (fn [props]
                     (when (meta-down?)
                       (reset-blink props)

                       (paste! props)))})

## bindings from key to function
(def default-binds @{:home (fn [props]
                             (reset-blink props)

                             (if (or (key-down? :left-shift)
                                     (key-down? :right-shift))
                               (select-to-start-of-line props)
                               (move-to-start-of-line props)))

                     :end (fn [props]
                            (if (or (key-down? :left-shift)
                                    (key-down? :right-shift))
                              (select-to-end-of-line props)
                              (move-to-end-of-line props)))

                     :z
                     (fn [props]
                       (cond
                         (and (or (key-down? :left-shift)
                                  (key-down? :right-shift))
                              (meta-down?))
                         (redo! props)

                         (meta-down?)
                         (undo! props)))

                     :left (fn [props]
                             (reset-blink props)

                             (cond
                               ## select whole words
                               (and (or (key-down? :left-alt)
                                        (key-down? :right-alt))
                                    (or (key-down? :left-shift)
                                        (key-down? :right-shift)))
                               (select-backward-word props)

                               (or (key-down? :left-alt)
                                   (key-down? :right-alt))
                               (backward-word props)

                               (or (key-down? :left-shift)
                                   (key-down? :right-shift))
                               (select-backward-char props)

                               (backward-char props)))

                     :right (fn [props]
                              (reset-blink props)

                              (cond
                                (and (or (key-down? :left-alt)
                                         (key-down? :right-alt))
                                     (or (key-down? :left-shift)
                                         (key-down? :right-shift)))
                                (select-forward-word props)

                                (or (key-down? :left-alt)
                                    (key-down? :right-alt))
                                (forward-word props)

                                (or (key-down? :left-shift)
                                    (key-down? :right-shift))
                                (select-forward-char props)

                                (forward-char props)))

                     :delete (fn [props]
                               (reset-blink props)

                               (cond (or (key-down? :left-alt)
                                         (key-down? :right-alt))
                                 (delete-word-forward! props)

                                 (delete-after-caret! props)))

                     :a (fn [props]
                          (when (meta-down?)
                            (select-all props)))

                     :x (fn [props]
                          (when (meta-down?)
                            (reset-blink props)

                            (cut! props)))

                     :c (fn [props]
                          (when (meta-down?)
                            (copy props)))

                     :v (kw->f :paste)

                     :backspace (fn [props]

                                  (reset-blink props)

                                  (cond (or (key-down? :left-alt)
                                            (key-down? :right-alt))
                                    (delete-word-backward! props)

                                    (backspace! props)))

                     :up (fn [props]
                           (reset-blink props)

                           (cond
                             (or (key-down? :left-shift)
                                 (key-down? :right-shift))
                             (select-move-up! props)

                             (move-up! props)))

                     :down (fn [props]
                             (reset-blink props)

                             (cond
                               (or (key-down? :left-shift)
                                   (key-down? :right-shift))
                               (select-move-down! props)

                               (move-down! props)))

                     #:up (vertical-move previous-row (fn [_] 0))

                     # :down (vertical-move
                     #         next-row
                     #         |(length (content $)))


                     :page-up (fn [props]
                                #(page-up props)
                                )

                     :page-down (fn [props]
                                  #(page-down props)
                                  )})

(def gb-binds @{})

(merge-into gb-binds
            default-binds
            @{:o (fn [props]
                   (when (meta-down?)
                     ((props :open-file) props)))

              :l (fn [props]
                   (when (meta-down?)
                     (save-and-dofile props)))

              :s
              (fn [props]
                (when (meta-down?)
                  ((props :save-file) props)))

              :f (fn [props]
                   (reset-blink props)

                   (cond (or (key-down? :left-control)
                             (key-down? :right-control))
                     (do (print "formatting!")
                       (format-code props))

                     (when (meta-down?)
                       ((props :search) props))))

              :q (fn [props]
                   (when (or (key-down? :left-control)
                             (key-down? :right-control))
                     (put (props :data) :quit true)))

              :enter (fn [props]
                       (reset-blink props)

                       (cond
                         (or (key-down? :left-control)
                             (key-down? :right-control))
                         (eval-it2 (get-in props [:context :top-env])
                                   (gb-get-last-sexp props))

                         (insert-char! props (first "\n"))))})

(def file-open-binds @{})

(merge-into file-open-binds default-binds
            {:escape
             (fn [props]
               (deselect props)
               (focus-other props :main))

             :enter (fn [props]
                      (reset-blink props)
                      ((props :on-enter) props (string ((commit! props) :text))))})

(comment
  (put gb-data :binds gb-binds))

(defn add-bind
  ``
  Take a button kw, and a function or a key in `kw->f`. Adds that key and function to the global bindings.
  
  Examples:
  `(add-bind :b :paste)`
  `(add-bind :c (fn [props] (pp props)))`
  `(add-bind :c (fn [props] (when (meta-down?) (print "meta-c!"))))`
  ``
  [binds button f-or-kw]
  (def f (if (keyword? f-or-kw) (kw->f f-or-kw) f-or-kw))
  (put binds button f))

(def game-binds @{})
(def pressed-game-binds @{})

(varfn handle-keyboard
  [data props dt]

  (def {:binds binds} props)

  (put props :data data)

  (var k (get-key-pressed))

  (while (not= 0 k)
    (reset-blink props)

    (unless (and (binds k)
                 ((binds k) props))
      (cond
        (or (key-down? :left-shift)
            (key-down? :right-shift))
        (insert-char-upper! props k)

        (insert-char! props k)))

    (scroll-to-focus props)

    (set k (get-key-pressed)))

  (loop [[k dl] :pairs delay-left
         :let [left ((update delay-left k - dt) k)]]
    (when (<= left 0)
      ((binds k) props)
      (put delay-left k repeat-delay)
      (scroll-to-focus props)))

  (loop [k :keys binds
         :when (not= k (keyword "("))]
    (when (key-released? k)
      (put delay-left k nil)
      (scroll-to-focus props))

    (when (key-pressed? k)
      (reset-blink props)
      (put delay-left k initial-delay)
      ((binds k) props)

      (scroll-to-focus props)))

  (loop [k :keys game-binds]
    (when (key-down? k)
      (print "wat?")
      ((game-binds k))))

  (loop [k :keys pressed-game-binds]
    (when (key-pressed? k)
      (print "wat 2?")
      ((pressed-game-binds k)))))

(varfn handle-scroll
  [props]
  (let [move (get-mouse-wheel-move)]
    (when (not= move 0)
      (update props :scroll |(min 0 (+ $ (* move 10))))
      (put props :changed-scroll true))))

(varfn get-mouse-pos
  [props [mx my]]

  (def {:lines lines
        :y-poses y-poses
        :line-flags line-flags
        :position position
        :offset offset
        :conf conf
        :width-of-last-line-number width-of-last-line-number
        :scroll scroll} props)

  (def {:mult mult} conf)

  (def [x-pos y-pos] position)
  (def [ox oy] offset)

  (def y-offset (+ oy y-pos (* (conf :mult) scroll)))

  (let [line-index (max 0 (dec (binary-search-closest y-poses |(compare my (+ $ y-offset)))))
        row-start-pos (if (= 0 line-index)
                        0
                        (lines (dec line-index)))
        row-end-pos (lines line-index)
        char-i (index-passing-middle-max-width props
                                               row-start-pos
                                               row-end-pos
                                               (- mx (* mult ox)
                                                  (* mult width-of-last-line-number)))

        flag (line-flags (max 0 (dec line-index)))]

    (cond (and (= flag :regular)
               (= row-start-pos char-i)) ## to the left of \n
      (inc char-i)

      char-i)))

(varfn handle-shift-mouse-down
  [props]
  (def {:lines lines
        :offset offset
        :position position
        :y-poses y-poses
        :sizes sizes
        :conf conf
        :scroll scroll} props)

  (def [ox oy] offset)
  (def [x-pos y-pos] position)
  (def pos (get-mouse-position))
  (def [x y] pos)

  (def y-offset (+ y-pos oy (* (conf :mult) scroll)))
  (def x-offset (+ x-pos ox))

  (if (nil? (props :selection))
    (-> props
        (put :selection (props :caret))
        (put-caret (get-mouse-pos props pos))
        (put :stickiness (if (< x x-offset) :down :right))
        (put :changed-selection true))

    (let [curr-pos (get-mouse-pos props pos)
          start (min (props :selection) (props :caret))
          stop (max (props :selection) (props :caret))]
      (-> props
          (put :selection
               (if (> curr-pos start)
                 start
                 stop))
          (put-caret curr-pos)
          (put :stickiness (if (< x x-offset) :down :right))
          (put :changed-selection true)))))

(varfn handle-mouse
  [mouse-data props]
  (def {:lines lines
        :offset offset
        :position position
        :y-poses y-poses
        :sizes sizes
        :conf conf
        :scroll scroll} props)

  (def [ox oy] offset)
  (def [x-pos y-pos] position)
  (def pos (get-mouse-position))
  (def [x y] pos)

  (def y-offset (+ y-pos oy (* (conf :mult) scroll)))
  (def x-offset (+ x-pos ox))

  (put mouse-data :just-double-clicked false)
  (put mouse-data :just-triple-clicked false)

  (when (mouse-button-released? 0)
    (put mouse-data :last-text-pos nil)
    (put mouse-data :just-down nil)
    (put mouse-data :recently-double-clicked nil)
    (put mouse-data :recently-triple-clicked nil)
    (put mouse-data :up-pos [x y])

    (comment
      (when (mouse-data :down-pos)
        (put mouse-data :selected-pos [(get-mouse-pos
                                         (mouse-data :down-pos)
                                         props
                                         text
                                         sizes
                                         ps
                                         rows
                                         x-offset
                                         y-offset)
                                       (get-mouse-pos
                                         pos
                                         props
                                         text
                                         sizes
                                         ps
                                         rows
                                         x-offset
                                         y-offset)]))))

  (when (mouse-button-pressed? 0)
    (reset-blink props)

    (when (and (mouse-data :down-time2)
               (> 0.4 (- (get-time) (mouse-data :down-time2))))
      (put mouse-data :just-triple-clicked true)
      (put mouse-data :recently-triple-clicked true))

    (when (and (mouse-data :down-time)
               (> 0.25 (- (get-time) (mouse-data :down-time))))
      (put mouse-data :just-double-clicked true)
      (put mouse-data :recently-double-clicked true)
      (put mouse-data :down-time2 (get-time))))

  (cond (mouse-data :just-triple-clicked)
    (select-region props ;(gb-find-surrounding-paragraph!
                            props
                            (mouse-data :down-index)))

    (and (mouse-data :just-double-clicked)
         (not (key-down? :left-shift))
         (not (key-down? :right-shift)))
    (select-region props ;(word-at-index props (mouse-data :down-index)))

    (or (mouse-data :recently-double-clicked)
        (mouse-data :recently-triple-clicked)) nil # don't start selecting until mouse is released again

    (mouse-button-down? 0)
    (do
      (when (nil? (mouse-data :last-text-pos))
        (put mouse-data :last-text-pos (+ (props :gap-start)
                                          (length (props :gap)))))

      (put mouse-data :down-time (get-time))

      (if (= nil (mouse-data :just-down))
        (do (put mouse-data :just-down true)
          (put mouse-data :down-pos [x y])
          (put mouse-data :down-index (get-mouse-pos props pos)))
        (put mouse-data :just-down false))

      (if (or (key-down? :left-shift)
              (key-down? :right-shift))
        (handle-shift-mouse-down props)

        (let [down-pos (mouse-data :down-index)
              curr-pos (get-mouse-pos props pos)]

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
              (put :changed-nav true))))

      (comment
        (var moved-caret false)

        (let [[start end] (mouse-data :selected-pos)
              start (if (or (key-down? :left-shift)
                            (key-down? :right-shift))
                      (mouse-data :last-text-pos)
                      start)

              single (= start end)
              p (dec (get-in mouse-data [:selected-pos 1])) ## position before mouse
              rp (row-of-pos (props :rows) p)
              newline (= (first "\n") (get both p))]

          (if (and single
                   newline
                   (= rp (y->row props (pos 1))))
            (select-region props (dec start) (dec end))
            (do
              (if (= (row-of-pos (props :rows) (get-in mouse-data [:selected-pos 1]))
                     (y->row props (pos 1)))
                (put props :stickiness :down)
                (put props :stickiness :right))

              (select-region props start end))))))))
