# (aoeaoedf "misc/frp4.janet")

(comment
  # (fset 'eval-df-thing (kmacro-lambda-form [S-right ?\( ?d ?f ?  ?\" ?m ?i ?s ?c ?/ ?f ?r ?p ?4 ?. ?j ?a ?n ?e ?t ?\" ?\) return S-left] 0 "%d"))
  # (global-set-key (kbd "C-c C-l") 'eval-df-thing)
)


(use jaylib)

(import ./../src/extra_channel :as ec :fresh true)
(import ./../src/events :as e :fresh true)
(import ./../src/state :as state)
(import ./../src/keyboard :as kb :fresh true)
(import ./../vector_math :as v :fresh true)
(import ./../src/input :as i)
(import ./../src/file_handling :prefix "")
(import ./../backwards2 :prefix "")
(import ./../src/render_new_gap_buffer :prefix "")
(import ./../src/new_gap_buffer :prefix "")

(def mouse (ev/chan 100))
(def chars (ev/chan 100))
(def keyboard (ev/chan 100))
(def callbacks (ev/chan 100))
(def frame-chan (ev/chan 1))
(def rerender (ev/chan 1))


(var delay-left @{})

(defn handle-keys
  [dt]
  (var k (get-char-pressed))

  (while (not= 0 k)
    (ec/push! chars @[:char k])
    (set k (get-char-pressed)))

  (loop [[k dl] :pairs delay-left
         :let [left ((update delay-left k - dt) k)]]
    (when (<= left 0)
      (ec/push! keyboard @[:key-down k])
      (put delay-left k i/repeat-delay)))

  (loop [k :in kb/possible-keys]
    (when (key-released? k)
      (put delay-left k nil))

    (when (key-pressed? k)
      (put delay-left k i/initial-delay)
      (ec/push! keyboard @[:key-down k]))))

(varfn handle-scroll
  []
  (let [move (get-mouse-wheel-move)]
    (when (not= move 0)
      (ec/push! mouse @[:scroll (* move 30) (get-mouse-position)]))))

# table of callbacks, eg @{@[:down [10 10]  [|(print "hello") |(print "other")]}
#                          ^ a mouse event  ^ queued callbacks
#                                           ^ is actually a ev/chan
#                                           ^ but using struct to visualise
(def callbacks @{:event/changed false})

(defn push-callback!
  [ev cb]
  (e/update! callbacks ev (fn [chan]
                            (default chan (ev/chan 1))
                            (ec/push! chan cb)
                            chan)))

(defn handle-callbacks
  [callbacks]
  (loop [[ev cbs] :pairs callbacks
         :when (not= ev :event/changed)]
    (e/pull-all cbs [apply]))

  (loop [k :in (keys callbacks)]
    (put callbacks k nil)))

(def button
  @{:rec [300 20 100 50]
    :color :green

    :cb (fn [self]
          (print "a " (self :color) " button was pressed"))

    :draw (fn draw-button
            [self]
            (draw-rectangle-rec (self :rec) (self :color)))

    :on-event (fn update-button
                [self ev]
                (def [kind data] ev)
                (case kind
                  :press (when (in-rec? data (self :rec))
                           (push-callback! ev |(:cb self)))))})

(def search-area @{})
(def file-open-area @{})

(def mouse-events {:press :press
                   :drag :drag
                   :release :release
                   :double-click :double-click
                   :triple-click :triple-click})

(defn text-area-on-event
  [self ev]

  (match ev
    [:key-down k]
    (do
      (i/handle-keyboard2
        (self :gb)
        k)
      (put self :event/changed true))
    [:char k]
    (do
      (i/handle-keyboard-char
        (self :gb)
        k)
      (put self :event/changed true))
    [:scroll n mp]
    (when (in-rec? mp
                   (i/gb-rec (self :gb)))

      (push-callback! ev (fn []
                           (i/handle-scroll-event (self :gb) n)
                           (put self :event/changed true))))

    ['(mouse-events (first ev)) _]
    (i/handle-mouse-event
      (self :gb)
      ev
      (fn [kind f]
        (push-callback! ev (fn []
                             (f)
                             (e/put! state/focus123 :focus self)
                             (put (self :gb) :event/changed true)))))))

(merge-into state/file-open-data {:binds i/file-open-binds})

(defn open-file
  [_]
  (e/put! state/focus123 :focus file-open-area))

(def text-area
  @{:id :main

    :gb (merge-into
          state/gb-data
          @{:binds i/gb-binds

            :search
            (fn [props]
              (e/put! state/focus123 :focus search-area))

            :open-file
            open-file})

    :draw (fn [self]
            (rl-pop-matrix)

            #(end-texture-mode)

            (gb-pre-render (self :gb))

            #(begin-texture-mode (rt-ref :data))

            (rl-push-matrix)

            (rl-load-identity)

            #(rl-scalef 2 2 1)

            (gb-render-text (self :gb)))

    :on-event (fn [self ev]
                (text-area-on-event self ev))})

(varfn search
  [props]
  (let [search-term (string (content props))]
    (put-caret state/gb-data (if (state/gb-data :selection)
                               (max (state/gb-data :selection)
                                    (state/gb-data :caret))
                               (state/gb-data :caret)))
    (when-let [i (gb-find-forward! state/gb-data search-term)]
      (-> state/gb-data
          (reset-blink)
          (put-caret i)
          (put :selection (gb-find-backward! state/gb-data search-term))
          (put :changed-selection true)))))

(varfn search-backward2
  [props]
  (let [search-term (string (content props))]
    (put-caret state/gb-data (if (state/gb-data :selection)
                               (min (state/gb-data :selection)
                                    (state/gb-data :caret))
                               (state/gb-data :caret)))
    (when-let [i (gb-find-backward! state/gb-data search-term)]
      (-> state/gb-data
          (reset-blink)
          (put-caret i)
          (put :selection (gb-find-forward! state/gb-data search-term))
          (put :changed-selection true)))))

(put state/search-data :binds
     @{:escape
       (fn [props]
         (put state/focus123 :focus text-area)
         (put state/focus123 :event/changed true))

       :enter search

       :control @{:f search

                  :b search-backward2}
       #
})

(table/setproto (state/search-data :binds) i/global-keys)

(merge-into
  search-area
  @{:id :search

    :gb state/search-data

    :draw (fn [self]
            (rl-pop-matrix)

            #(end-texture-mode)

            (gb-pre-render (self :gb))

            #(begin-texture-mode (rt-ref :data))

            (rl-push-matrix)

            (rl-load-identity)

            #(rl-scalef 2 2 1)

            (gb-render-text (self :gb)))

    :on-event (fn [self ev]
                (text-area-on-event self ev))})

(merge-into
  file-open-area
  @{:id :file-open

    :gb state/file-open-data

    :draw (fn [self]
            (rl-pop-matrix)

            #(end-texture-mode)

            (gb-pre-render (self :gb))

            #(begin-texture-mode (rt-ref :data))

            (rl-push-matrix)

            (rl-load-identity)

            #(rl-scalef 2 2 1)

            (gb-render-text (self :gb)))

    :on-event (fn [self ev]
                (text-area-on-event self ev))})


(merge-into (state/file-open-data :binds)
            {:escape
             (fn [props]
               (e/put! state/focus123 :focus text-area))

             :enter (fn [props]
                      (load-file state/gb-data (string ((commit! props) :text)))
                      (e/put! state/focus123 :focus text-area))})

(def caret
  @{:draw (fn [self]
            (when-let [gb (and (self :on)
                               (self :gb))]
              (render-cursor gb)))

    :on true

    :on-event (fn [self ev]
                (match ev
                  {:focus state/focus123}
                  (when (get-in state/focus123 [:gb :gap])
                    (put self :gb (state/focus123 :gb))
                    (set ((self :gb) :blink) 0)
                    (put self :on true))

                  [:dt dt]
                  (when (self :gb)
                    (update (self :gb) :blink + dt)

                    (when (and (> ((self :gb) :blink) 0.6)
                               (self :on))
                      (put self :on false))

                    (when (> ((self :gb) :blink) 1.0)
                      (set ((self :gb) :blink) 0)
                      (put self :on true)))))})

(def button2
  (table/setproto
    @{:rec [350 30 100 50]
      :color :blue}
    button))

(comment
  (get-in state/focus123 [:focus :id])

  (loop [[pullable pullers] :pairs dependencies]
    (when-let [hi-i (find-index |(and (table? $) ($ :history)) pullers)]
      (def history (pullers hi-i))
      (array/remove pullers hi-i)
      (case (type pullable)
        :core/channel (e/pull-all (history :history) [pullable])
        :table (do (loop [k :in (keys pullable)]
                     (put pullable k nil))
                 (merge-into pullable (history :history)))
        (error (string "Strange" (type pullable))))
      (array/push pullers history)))
  #
)

# (e/record-all dependencies)
# need to make gb-data not contain circular references etc

(def mouse-data (i/new-mouse-data))

(varfn handle-mouse
  [mouse-data]
  (def pos (get-mouse-position))
  (def [x y] pos)

  (put mouse-data :just-double-clicked false)
  (put mouse-data :just-triple-clicked false)

  (when (mouse-button-released? 0)
    (put mouse-data :just-down nil)
    (put mouse-data :recently-double-clicked nil)
    (put mouse-data :recently-triple-clicked nil)
    (put mouse-data :up-pos [x y])

    (ec/push! mouse @[:release (get-mouse-position)]))

  (when (mouse-button-pressed? 0)
    (when (and (mouse-data :down-time2)
               # max time to pass for triple click
               (> 0.4 (- (get-time) (mouse-data :down-time2)))
               # max distance to travel for triple click
               (> 200 (v/dist-sqr pos (mouse-data :down-pos))))
      (put mouse-data :just-triple-clicked true)
      (put mouse-data :recently-triple-clicked true))

    (when (and (mouse-data :down-time)
               # max time to pass for double click
               (> 0.25 (- (get-time) (mouse-data :down-time)))
               # max distance to travel for double click
               (> 100 (v/dist-sqr pos (mouse-data :down-pos))))
      (put mouse-data :just-double-clicked true)
      (put mouse-data :recently-double-clicked true)
      (put mouse-data :down-time2 (get-time))))

  (cond (mouse-data :just-triple-clicked)
    (ec/push! mouse @[:triple-click (get-mouse-position)])

    (and (mouse-data :just-double-clicked)
         (not (key-down? :left-shift))
         (not (key-down? :right-shift)))
    (ec/push! mouse @[:double-click (get-mouse-position)])

    (or (mouse-data :recently-double-clicked)
        (mouse-data :recently-triple-clicked))
    nil # don't start selecting until mouse is released again

    (mouse-button-down? 0)
    (do
      (put mouse-data :down-time (get-time))

      (if (= nil (mouse-data :just-down))
        (do (put mouse-data :just-down true)
          (put mouse-data :last-pos pos)
          (put mouse-data :down-pos pos)
          (ec/push! mouse @[:press (get-mouse-position)]))
        (do (put mouse-data :just-down false)
          (unless (= pos (mouse-data :last-pos))
            (put mouse-data :last-pos pos)
            (ec/push! mouse @[:drag (get-mouse-position)])))))))

(var deps @{})

(def dependencies
  @{mouse @[text-area search-area file-open-area]
    keyboard @[|(:on-event (state/focus123 :focus) $)]
    chars @[|(:on-event (state/focus123 :focus) $)]
    state/focus123 @[caret]
    callbacks @[handle-callbacks]})

(def draws @[|(:draw text-area)
             |(case (state/focus123 :focus)
                search-area (:draw search-area)
                file-open-area (:draw file-open-area))
             |(:draw caret)])

(varfn render-deps
  [dt]
  (loop [d :in (deps :draws)]
    (d)))

(def finally
  @{frame-chan [render-deps caret]})

(merge-into deps @{:deps dependencies
                   :draws draws
                   :finally finally})

(varfn trigger
  [dt]
  (handle-keys dt)
  (handle-scroll)

  (ec/push! frame-chan @[:dt dt])

  (handle-mouse mouse-data)

  (comment
    (when (mouse-button-pressed? 0)
      # uses arrays in order to have reference identity rather than value identity
      # relevant for callback handling
      (ec/push! mouse @[:press (get-mouse-position)])))

  (e/pull-deps (deps :deps) (deps :finally)))

(comment
  (ec/push! mouse [:down [10 10]]))
