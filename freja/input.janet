(use freja-jaylib)
(import ./eval :prefix "")
(import freja/state)
(import ./dumb :prefix "")
(import ./new_gap_buffer :as gb)
(import ./render_new_gap_buffer :as render-gb)
(import ./file-handling :as fh)
(import ./code_api :prefix "")
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")
(import ./collision :prefix "")

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
  [env code]
  (print "Evaling! " code)
  (try (do (fiber/setenv (fiber/current) state/user-env)
         (def res (eval-string code))
         (pp res))
    ([err fib]
      (debug/stacktrace fib err))))

(varfn search2
  [props]
  (:search props))

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

(def undo!2 (comp reset-blink gb/undo!))
(def paste! (comp reset-blink gb/paste!))
(def cut! (comp reset-blink gb/cut!))
(def redo! (comp reset-blink gb/redo!))
(def format! (comp reset-blink format-code))
(def select-backward-word (comp reset-blink gb/select-backward-word))
(def select-forward-word (comp reset-blink gb/select-forward-word))
(def delete-word-backward! (comp reset-blink gb/delete-word-backward!))
(def delete-word-forward! (comp reset-blink gb/delete-word-forward!))
(def backward-word (comp reset-blink gb/backward-word))
(def forward-word (comp reset-blink gb/forward-word))
(def select-backward-char (comp reset-blink gb/select-backward-char))
(def select-forward-char (comp reset-blink gb/select-forward-char))
(def backward-char (comp reset-blink gb/backward-char))
(def forward-char (comp reset-blink gb/forward-char))
(def move-to-start-of-line (comp reset-blink render-gb/move-to-start-of-line))
(def move-to-end-of-line (comp reset-blink render-gb/move-to-end-of-line))
(def delete-after-caret! (comp reset-blink gb/delete-after-caret!))
(def delete-before-caret! (comp reset-blink gb/delete-before-caret!))
(def move-up! (comp reset-blink render-gb/move-up!))
(def move-down! (comp reset-blink render-gb/move-down!))

(def global-keys
  @{:alt @{:shift @{:left select-backward-word
                    :right select-forward-word
                    #
}

           :backspace delete-word-backward!
           :delete delete-word-forward!

           :left backward-word
           :right forward-word
           #
}

    :control @{:a gb/select-all
               :x cut!
               :c gb/copy
               :v paste!
               :z undo!2
               :y redo!}

    :shift @{:home render-gb/select-to-start-of-line
             :end render-gb/select-to-end-of-line
             :left select-backward-char
             :right select-forward-char
             :up render-gb/select-move-up!
             :down render-gb/select-move-down!

             #
}

    :left backward-char
    :right forward-char
    :up move-up!
    :down move-down!

    :home move-to-start-of-line
    :end move-to-end-of-line

    :delete delete-after-caret!
    :backspace delete-before-caret!

    #
})

(defn quit
  [props]
  (put (props :context) :quit true))

(defn open-file
  [props]
  (:open-file props))

(defn save-file
  [& args]
  (fh/save-file ;args))

(def gb-binds @{:control @{:shift @{:f format!
                                    #
}

                           :f search2
                           :o open-file
                           :l fh/save-and-dofile
                           :s save-file
                           :q quit

                           :enter |(eval-it (get-in $ [:context :top-env])
                                            (gb-get-last-sexp $))
                           #
}
                :enter (comp reset-blink |(gb/insert-char! $ (chr "\n")))})

(table/setproto gb-binds global-keys)

(def file-open-binds @{:escape
                       (fn [props]
                         (gb/deselect props))

                       :enter (fn [props]
                                (reset-blink props)
                                ((props :on-enter) props (string ((gb/commit! props) :text))))})

(table/setproto file-open-binds global-keys)

(varfn handle-keyboard-char
  [props k]

  (def {:binds binds} props)

  # TODO: Need to add get-char-pressed

  (reset-blink props)

  (cond
    (or (key-down? :left-shift)
        (key-down? :right-shift))
    (gb/insert-char-upper! props k)

    (gb/insert-char! props k))

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
                      (* (screen-scale 1)))]
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
          char-i (render-gb/index-passing-middle-max-width
                   props
                   row-start-pos
                   row-end-pos
                   (- mx (* mult ox)
                      (* mult x-pos)
                      (* mult width-of-last-line-number)))

          flag (line-flags (max 0 (dec line-index)))]

      (min
        (gb/gb-length props)
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

(varfn gb-rec
  [{:offset offset
    :position position
    :size size
    :scroll scroll
    :conf conf}]

  (def [ox oy] offset)
  (def [x-pos y-pos] position)

  (def y-offset (+ y-pos oy)) # (* (conf :mult) scroll)))
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
      (cb kind |(gb/select-region props ;(gb/find-surrounding-paragraph!
                                           props
                                           (get-mouse-pos props mouse-pos))))
      #                       should maybe remember original pos

      (and (= kind :double-click)
           (not (key-down? :left-shift))
           (not (key-down? :right-shift)))
      (cb kind |(gb/select-region props ;(gb/word-at-index props
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
