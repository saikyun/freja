(import freja-layout/default-tags :as dt)
(import ./new_gap_buffer :as gb)
(import ./input :as i)
(import ./collision :as c)
(import freja/theme)
(import freja/frp)
(import ./events :as e)
(import freja/state)
(import ./render_new_gap_buffer :as rgb)
(use freja-jaylib)

(use profiling/profile)

(defn remove-partial-voiced-part
  [gb]
  (def vs (or (gb :voice-partial-start)
              (gb :voice-start)))

  (def vl (or (gb :voice-partial-gb-length)
              (gb :voice-gb-length)))

  (def stop
    (+ vs (- (gb/gb-length gb) vl)))

  (if (< stop vs)
    (gb/put-caret gb vs)
    (-> gb
        (gb/delete-region! vs stop)
        (gb/put-caret vs))))


# just doing this as inlining
(defmacro press
  [kind]
  ~(do (i/handle-keyboard2
         (self :gb)
         k
         ,kind)
     (put self :event/changed true)))

(defn symbols-etc
  [delim]
  (->>
    {"one" "1"}
    pairs
    (map (fn [[k v]]
           [(keyword k)
            ~(/ ,k [:insert ,(string v delim)])]))
    from-pairs))

(def literal-peg
  (-> ~@{:delim (+ " " -1)
         :slap (/ "slap" [:insert "\n"])
         :janet (/ "janet mode" [:set-mode :janet])
         :word (/ '(some (if-not :delim 1)) ,(fn [sym] [:insert sym " "]))
         :main (some (* (+ :janet
                           :slap
                           "literal mode"
                           ,;(keys (symbols-etc " "))
                           :word) :delim))}

      (merge-into (symbols-etc " "))

      peg/compile))

(def dashed-literal-peg
  (-> ~@{:delim (+ " " -1)
         :slap (/ "slap" ,(fn [] [[:backspace]
                                  [:insert " "]
                                  [:set-mode :janet]]))
         :janet (/ "janet mode" [:set-mode :janet])
         :word (/ '(some (if-not :delim 1)) ,(fn [sym] [:insert sym "-"]))
         :main (some (* (+ :janet
                           :slap
                           "kebab"
                           ,;(keys (symbols-etc "-"))
                           :word) :delim))}

      (merge-into (symbols-etc "-"))

      peg/compile))

(def janet-peg
  (-> ~@{:call (/ "call" ,(fn [] [[:insert "()"]
                                  [:move-caret -1]]))
         :funk (/ "funk" ,(fn [] [[:insert "(defn )"]
                                  [:move-caret -1]]))
         :define (/ "define" ,(fn [] [[:insert "(def )"]
                                      [:move-caret -1]]))
         :braces (/ "braces" ,(fn [] [[:insert "[]"]
                                      [:move-caret -1]]))
         :symbol (/ '(some (if-not :delim 1)) ,(fn [sym] [:insert sym " "]))
         :keyword (/ "keyword" ,(fn [] [[:insert ":"]
                                        [:set-mode :dashed-literal]]))
         :dash (/ "dash" ,(fn [] [[:backspace-whitespace]
                                  [:insert "-"]]))
         :delim (+ " " -1)
         :slap (/ "slap" ,(fn [] [[:backspace-whitespace]
                                  [:exit-expr]]))
         :salmon (/ "salmon" ,(fn [] [:down-back-expr]))
         :plus (/ "plus" [:insert "+ "])

         :one (/ "one" [:insert "1 "])

         :literal (/ "literal mode" [:set-mode :literal])

         :dashed-literal (/ "kebab" [:set-mode :dashed-literal])

         :main (some (* (+ :slap
                           :salmon
                           :funk
                           :define
                           "janet mode"
                           :dashed-literal
                           :call
                           :dash
                           :braces
                           :one
                           :plus
                           :keyword
                           :literal
                           ,;(keys (symbols-etc " "))
                           :symbol) :delim))}

      (merge-into (symbols-etc " "))

      peg/compile))

(defn resolve-step
  [gb step]
  (match step
    [:insert & ss]
    (loop [s :in ss]
      (gb/insert-string-at-caret! gb s))

    [:move-caret n]
    (gb/update-caret gb + n)

    [:set-mode m]
    (put gb :voice/next-mode (tracev m))

    [:backspace]
    (gb/backspace! gb)

    [:backspace-whitespace]
    (let [p (gb/gb-find-backward! gb '(if-not " " 1))
          c (gb :caret)
          t (inc p)]
      (-> gb
          (gb/delete-region! t c)
          (gb/put-caret t)))

    [:exit-expr]
    (let [t (gb/gb-find-forward! gb '(+ (some "`")
                                        (set `})]"`)))]
      (gb/put-caret gb t))

    [:down-back-expr]
    (let [t (gb/gb-find-backward! gb '(+ (some "`")
                                         (set `})]"`)))]
      (gb/put-caret gb t))

    x (errorf "undefined step %p" step)))

(defn resolve-steps
  [gb steps]
  (loop [s :in steps]
    (cond (indexed? (first s))
      (resolve-steps gb s)

      :else
      (resolve-step gb s))))

(comment
  (def gb (get-in (last (get-in state/editor-state [:stack])) [1 :editor :gb]))
  (def steps (peg/match janet-peg "call filter call fn"))
  (def steps2 (peg/match janet-peg "hurp dash cat"))
  #(pp steps)
  #(pp steps2)
)


(varfn keyword-before?
  "Gets the position of the start of the word before the caret, if it is a keyword.
Does not skip spaces."
  [gb]
  (def {:caret caret} gb)

  (var target-i nil)

  (def f (fiber/new (fn [] (gb/index-char-backward-start gb (dec caret)))))

  (loop [[i c] :iterate (resume f)]
    (case c
      (chr " ") (break)

      (chr ":") (do (set target-i i)
                  (break))))

  target-i)

(defn keyword-mode?
  [gb]
  (keyword-before? gb))

(comment
  (resolve-steps gb steps)
  (resolve-steps gb steps2))

(defn voice-transform
  [peg t]
  (peg/match peg t))

#nstoeuathnsueohntsaushneo

(defn remove-voiced-part
  [gb]

  (def vs (or #(gb :voice-partial-start)
              (gb :voice-start)))

  (def vl (or #(gb :voice-partial-gb-length)
              (gb :voice-gb-length)))

  (def stop (+ vs (- (gb/gb-length gb) vl)))

  (if (< stop vs)
    (gb/put-caret vs)
    (-> gb
        (gb/delete-region! vs stop)
        (gb/put-caret vs))))


(defn select-partial-voiced-part
  [gb]

  (def vs (or (gb :voice-partial-start)
              (gb :voice-start)))

  (def vl (or (gb :voice-partial-gb-length)
              (gb :voice-gb-length)))

  (gb/select-region gb vs
                    (+ vs (- (gb/gb-length gb) vl))))

(defn select-voiced-part
  [gb]
  (-> gb
      (gb/select-region
        (gb :voice-start)
        (+ (gb :voice-start)
           (- (gb/gb-length gb)
              (gb :voice-gb-length))))))

(defn on-partial
  [gb peg t]
  (-> gb
      remove-partial-voiced-part
      (put :voice-partial-start (gb :caret))
      (put :voice-partial-gb-length (gb/gb-length gb)))

  (let [res (voice-transform peg t)]
    (resolve-steps gb res))

  (select-partial-voiced-part gb))

(defn on-text
  [gb peg t]
  (let [steps (voice-transform peg t)]
    (remove-partial-voiced-part gb)
    (resolve-steps gb steps)
    (put gb :voice-partial-start (gb :caret))
    (put gb :voice-partial-gb-length (gb/gb-length gb))))

(defn text-area-on-event
  [self ev]
  (def gb (self :gb))

  (match ev
    [:voice-text-literal t]
    (when (or (nil? (gb :voice/mode))
              (= :literal (gb :voice/mode))
              (= :dashed-literal (gb :voice/mode)))
      (on-text gb (case (gb :voice/mode)
                    :dashed-literal dashed-literal-peg
                    literal-peg) t)

      (when-let [m (gb :voice/next-mode)]
        (put gb :voice/mode m)
        (print "voice mode is now: " m)
        (put gb :voice/next-mode nil)))

    [:voice-partial-literal t]
    (when (or (nil? (gb :voice/mode))
              (= :literal (gb :voice/mode))
              (= :dashed-literal (gb :voice/mode)))
      (on-partial gb (case (gb :voice/mode)
                       :dashed-literal dashed-literal-peg
                       literal-peg) t))

    [:voice-partial t]
    (when (= :janet (gb :voice/mode))
      (on-partial gb janet-peg t))

    [:voice-text t]
    (when (= :janet (gb :voice/mode))
      (on-text gb janet-peg t)

      (when-let [m (gb :voice/next-mode)]
        (put gb :voice/mode m)
        (print "voice mode is now: " m)
        (put gb :voice/next-mode nil)))

    [:voice-stack t]
    (-> gb
        (put :voice-start nil)
        (put :voice-partial-start nil)
        (put :voice-gb-length nil)
        (put :voice-partial-gb-length nil))

    [:key-down k]
    (press :key-down)

    [:key-repeat k]
    (press :key-repeat)

    [:key-release k]
    (press :key-release)

    [:char k]
    (do
      (i/handle-keyboard-char
        (self :gb)
        k)
      (put self :event/changed true))

    [:scroll n mp]
    (when (c/in-rec? mp
                     (i/gb-rec (self :gb)))
      (i/handle-scroll-event (self :gb) n)
      (put self :event/changed true))

    [(_ (i/mouse-events (first ev))) _]
    (i/handle-mouse-event
      (self :gb)
      ev
      (fn [kind f]
        (f)
        (state/focus! self)
        (put (self :gb) :event/changed true)))))

(varfn draw-textarea
  [self]
  (def {:gb gb} self)

  (def {:size size} gb)

  (when size
    (draw-rectangle
      0
      0
      (in size 0)
      (in size 1)
      (or (gb :background)
          (get-in gb [:colors :background])
          :blank)))

  (rgb/gb-pre-render gb)
  #  (rgb/inner-render gb)

  #(print "huh")

  (rgb/gb-render-text gb)

  (when (= self (state/focus :focus))
    (when (> 30 (gb :blink))
      (rgb/render-cursor gb))

    (update gb :blink inc) # TODO: should be dt

    (when (< 50 (gb :blink))
      (put gb :blink 0))))

(defn default-textarea-state
  [&keys {:gap-buffer gap-buffer
          :binds binds
          :on-change on-change}]
  (default gap-buffer (gb/new-gap-buffer))

  (default binds (table/setproto @{} state/gb-binds))

  (merge-into gap-buffer
              {:binds binds
               :colors theme/colors})

  (update gap-buffer :blink |(or $ 0))

  @{:gb gap-buffer

    # call like this so varfn works
    :draw (fn [self] (draw-textarea self))

    :on-event (fn [self ev]
                (text-area-on-event self ev)
                (when (and on-change
                           (get-in self [:gb :changed]))
                  (on-change (gb/content (self :gb)))))})

(defn textarea
  [props & _]
  (def {:state state
        :offset offset
        :text/color text/color
        :text/size text/size
        :text/font text/font
        :text/line-height text/line-height
        :text/spacing text/spacing
        :background bg
        :binds binds #replaces binds
        :extra-binds extra-binds #adds onto default binds
        :show-line-numbers show-line-numbers
        :on-change on-change
        :text text} props)

  (default state (get (dyn :element) :state @{}))

  (unless (get state :gb)
    (merge-into state (default-textarea-state
                        :on-change on-change
                        :binds binds
                        :extra-binds extra-binds)))

  (when (table? props)
    (put props :internal-gb (get state :gb)))

  (when text
    (-> (state :gb)
        (gb/replace-content text)
        (gb/end-of-buffer)))

  (when bg
    (put-in state [:gb :background] bg))

  (when extra-binds
    (put (state :gb)
         :binds
         (-> (merge-into @{} extra-binds)
             (table/setproto state/gb-binds))))

  (default offset (if show-line-numbers
                    [12 0]
                    [12 0]))
  (default text/size (dyn :text/size 14))
  (default text/font (dyn :text/font "Poppins"))
  (default text/line-height (dyn :text/line-height 1))
  (default text/spacing (dyn :text/spacing 1))
  (default text/color (dyn :text/color 0x000000ff))

  (put-in state [:gb :text/size] text/size)
  (put-in state [:gb :text/font] text/font)
  (put-in state [:gb :text/line-height] text/line-height)
  (put-in state [:gb :text/spacing] text/spacing)
  (put-in state [:gb :text/color] text/color)
  (put-in state [:gb :changed] true)
  (put-in state [:gb :show-line-numbers] show-line-numbers)
  (put-in state [:gb :offset] offset)

  (-> (dyn :element)
      (dt/add-default-props props)
      (put :state state)
      (merge-into
        @{:children []
          :relative-sizing
          (fn [el max-width max-height]
            # (print "resizing text area " max-width " " max-height)
            # TODO: something strange happens when width / height is too small
            # try removing 50 then resize to see
            (-> el
                (put :width (max 50 (or (el :preset-width) max-width)))
                (put :height (max (get-in state [:gb :conf :size] 0)
                                  (or (el :preset-height) max-height)))
                (put :content-width (el :width))
                (put :layout/lines nil))

            (def [old-w old-h] (get-in state [:gb :size]))

            (unless (and (= old-w (el :width))
                         (= old-h (el :height)))
              (put-in state [:gb :size]
                      [(math/floor (el :width))
                       (math/floor (el :height))])
              (put-in state [:gb :changed] true)
              (put-in state [:gb :resized] true))

            # (print "el: " (el :width) " / " (el :height))

            el)

          :render (fn [self parent-x parent-y]
                    (:draw state))

          :on-event (fn [self ev]
                      #(pp self)
                      #(print "start " (state :id))

                      #(tracev [(dyn :offset-x) (dyn :offset-y)])

                      (defn update-pos
                        [[x y]]
                        [(- x
                            (dyn :offset-x 0))
                         (- y
                            (dyn :offset-y 0))])

                      (def new-ev (if (= (first ev) :scroll)
                                    [(ev 0)
                                     (ev 1)
                                     (update-pos (ev 2))]
                                    [(ev 0)
                                     (update-pos (ev 1))]))

                      #(text-area-on-event state new-ev)
                      (:on-event state new-ev)

                      (when (and on-change
                                 (get-in state [:gb :changed]))
                        (on-change (gb/content (state :gb))))

                      (def pos (new-ev
                                 (if (= :scroll (first new-ev))
                                   2
                                   1)))

                      (when (dt/in-rec? pos
                                        0
                                        0
                                        (self :width)
                                        (self :height))
                        true))})))
