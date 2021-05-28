(use jaylib)
(import ./eval :prefix "")
(import ./state :prefix "")
(import ./new_gap_buffer :prefix "")
(import ./render_new_gap_buffer :prefix "")
(import ./file_handling :prefix "")
(import ./code_api :prefix "")
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")
(import ./../backwards2 :prefix "")

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

(varfn unfocus
  [{:context context} id]
  (set delay-left @{})
  (put context :focus nil))

## delay before first repetition of held keys
(var initial-delay 0.2)

## delay of each repetition thereafter
(var repeat-delay 0.03)

(def kw->f {:paste (fn [props]
                     (when (meta-down?)
                       (reset-blink props)

                       (paste! props)))})

(def global-keys
  @{:alt @{:shift @{:left (comp reset-blink select-backward-word)
                    :right (comp reset-blink select-forward-word)
                    #
}

           :backspace (comp reset-blink delete-word-backward!)
           :delete (comp reset-blink delete-word-forward!)

           :left (comp reset-blink backward-word)
           :right (comp reset-blink forward-word)
           #
}

    :control @{:a select-all
               :x (comp reset-blink cut!)
               :c copy
               :v (comp reset-blink paste!)
               :z (comp reset-blink undo!)
               :y (comp reset-blink redo!)}

    :shift @{:home select-to-start-of-line
             :end select-to-end-of-line
             :left (comp reset-blink select-backward-char)
             :right (comp reset-blink select-forward-char)
             :up select-move-up!
             :down select-move-down!

             #
}

    :left (comp reset-blink backward-char)
    :right (comp reset-blink forward-char)
    :up move-up!
    :down move-down!

    :home (comp reset-blink move-to-start-of-line)
    :end (comp reset-blink move-to-end-of-line)

    :delete (comp reset-blink delete-after-caret!)
    :backspace (comp reset-blink backspace!)

    #
})

(def gb-binds @{:control @{:shift @{:f (comp reset-blink format-code)
                                    #
}

                           :f |(:search $)
                           :o |(:open-file $)
                           :l save-and-dofile
                           :s save-file
                           :q |(put ($ :data) :quit true)

                           :enter |(eval-it2 (get-in $ [:context :top-env])
                                             (gb-get-last-sexp $))
                           #
}
                :enter (comp reset-blink |(insert-char! $ (chr "\n")))})

(table/setproto gb-binds global-keys)

(def file-open-binds @{:escape
                       (fn [props]
                         (deselect props)
                         (swap! focus-ref (fn [_] :main))
                         #(unfocus props)
)

                       :enter (fn [props]
                                (reset-blink props)
                                ((props :on-enter) props (string ((commit! props) :text))))})

(table/setproto file-open-binds global-keys)

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
  [props data]

  (print "handling kb (should not happen)")

  (def dt (data :dt))

  (def {:binds binds} props)

  (put props :data data)

  (var k (get-char-pressed))

  # TODO: Need to add get-char-pressed

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

    (set k (get-char-pressed)))

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

(varfn handle-keyboard-char
  [props k]

  (def {:binds binds} props)

  # TODO: Need to add get-char-pressed

  (reset-blink props)

  (cond
    (or (key-down? :left-shift)
        (key-down? :right-shift))
    (insert-char-upper! props k)

    (insert-char! props k))

  (scroll-to-focus props))

(def modifiers [:caps-lock :control :alt :shift #:meta
])

(def check-modifiers
  {:caps-lock |(key-down? :caps-lock)
   :control |(or (key-down? :left-control)
                 (key-down? :right-control))
   :shift |(or (key-down? :left-shift)
               (key-down? :right-shift))
   # :meta meta-down?
   :alt |(or (key-down? :left-alt)
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

(def global-set-key (partial set-key global-keys))

(defn hotkey-triggered
  [kmap k-down]
  (def mods (seq [m :in modifiers
                  :when ((check-modifiers m))]
              m))
  (var ret-f nil)
  (loop [[k f] :pairs (or (get-in kmap mods) [])
         :when (function? f) # on partial mod-combination, f will be a table
         :when (= k-down k)]
    (set ret-f f)
    (break))

  (if ret-f ret-f
    (when-let [p (and (not ret-f)
                      (table/getproto kmap))]
      (hotkey-triggered p k-down))))


(varfn handle-keyboard2
  [props k]
  (def {:binds binds} props)

  # TODO: Need to add get-char-pressed

  (when-let [f (hotkey-triggered (props :binds) k)]
    (f props))

  (comment
    (when-let [f (binds k)]
      (reset-blink props)
      (f props)

      (scroll-to-focus props))))

(varfn handle-scroll-event
  [props move]
  (if-let [max-y (-?> (props :y-poses)
                      last
                      -
                      (* ((props :screen-scale) 1)))]
    (update props :scroll |(max max-y (min 0 (+ $ (* move 2)))))
    (update props :scroll |(min 0 (+ $ (* move 2)))))

  (put props :changed-scroll true))

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
          char-i (index-passing-middle-max-width
                   props
                   row-start-pos
                   row-end-pos
                   (- mx (* mult ox)
                      (* mult x-pos)
                      (* mult width-of-last-line-number)))

          flag (line-flags (max 0 (dec line-index)))]

      (min
        (gb-length props)
        (cond (and (= flag :regular)
                   (= row-start-pos char-i)) ## to the left of \n
          (inc char-i)
          char-i)))))

(varfn handle-shift-mouse-down
  [props [kind mouse-pos] cb]
  (def {:lines lines
        :offset offset
        :position position
        :y-poses y-poses
        :sizes sizes
        :conf conf
        :scroll scroll} props)

  (def [ox oy] offset)
  (def [x-pos y-pos] position)

  (def [x y] mouse-pos)

  (def y-offset (+ y-pos oy (* (conf :mult) scroll)))
  (def x-offset (+ x-pos ox))

  (if (nil? (props :selection))
    (cb kind |(-> props
                  (put :selection (props :caret))
                  (put-caret (get-mouse-pos props mouse-pos))
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
                    (put-caret curr-pos)
                    (put :stickiness (if (< x x-offset) :down :right))
                    (put :changed-selection true))))))

(varfn gb-rec
  [{:offset offset
    :position position
    :size size
    :scroll scroll
    :conf conf}]

  (def [ox oy] offset)
  (def [x-pos y-pos] position)

  (def y-offset (+ y-pos oy (* (conf :mult) scroll)))
  (def x-offset (+ x-pos ox))

  [x-offset
   y-offset
   (match (size 0)
     :max (- (get-screen-height) y-offset)
     w w)
   (match (size 1)
     :max (- (get-screen-height) y-offset)
     h h)])

(varfn handle-mouse-event
  [props event cb]
  (def {:lines lines
        :offset offset
        :position position
        :y-poses y-poses
        :size size
        :sizes sizes
        :conf conf
        :scroll scroll} props)
  (def [kind mouse-pos] event)
  (def [x y] mouse-pos)

  (def [ox oy] offset)
  (def [x-pos y-pos] position)

  (def y-offset (+ y-pos oy (* (conf :mult) scroll)))
  (def x-offset (+ x-pos ox))

  (when (in-rec? mouse-pos
                 (gb-rec props))

    (cond
      (= kind :release)
      (cb kind |(put props :down-index nil))

      (= kind :triple-click)
      (cb kind |(select-region props ;(gb-find-surrounding-paragraph!
                                        props
                                        (get-mouse-pos props mouse-pos))))
      #                       should maybe remember original pos

      (and (= kind :double-click)
           (not (key-down? :left-shift))
           (not (key-down? :right-shift)))
      (cb kind |(select-region props ;(word-at-index props
                                                     (get-mouse-pos props mouse-pos))))
      #                                    should maybe remember original pos

      (and (or (= kind :press)
               (= kind :drag))
           (or (key-down? :left-shift)
               (key-down? :right-shift)))
      (handle-shift-mouse-down props event cb)

      (= kind :press)
      (cb kind |(let [cur-index (get-mouse-pos props mouse-pos)]
                  (-> props
                      reset-blink
                      (put :down-index cur-index)
                      (put :selection nil)
                      (put :changed-selection true)
                      (put :caret cur-index)
                      (put :stickiness (if (< x x-offset) :down :right))
                      (put :changed-nav true))))

      (= kind :drag)
      (cb kind |(let [down-pos (props :down-index)
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
