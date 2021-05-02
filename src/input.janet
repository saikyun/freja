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
                         (do
                           (print "undo")
                           (undo! props))))

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
               (swap! focus-ref (fn [_] :main))
               #(unfocus props)
               )
             
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
  
  (scroll-to-focus props)
  )

(varfn handle-keyboard2
  [props k]
  
  (def {:binds binds} props)
  
  # TODO: Need to add get-char-pressed
  
  (when-let [f (binds k)]
    (reset-blink props)
    (f props)
    
    (scroll-to-focus props)))

(varfn handle-scroll-event
  [props move]
  (update props :scroll |(min 0 (+ $ (* move 2))))
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

    (min
      (gb-length props)
      (cond (and (= flag :regular)
                 (= row-start-pos char-i)) ## to the left of \n
        (inc char-i)
        char-i))))

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

(varfn handle-mouse-event
  [props event cb]
  (def {:lines lines
        :offset offset
        :position position
        :y-poses y-poses
        :sizes sizes
        :conf conf
        :scroll scroll} props)
  (def [kind mouse-pos] event)
  (def [x y] mouse-pos)
  
  (def [ox oy] offset)
  (def [x-pos y-pos] position)
  
  (def y-offset (+ y-pos oy (* (conf :mult) scroll)))
  (def x-offset (+ x-pos ox))
  
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
                    (put :changed-nav true))))))
