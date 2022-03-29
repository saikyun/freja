# docs: https://github.com/saikyun/freja/wiki/%22frp%22

(import freja-jaylib :as jay)
(import ./events :as e :fresh true)
(import ./state :as state)
(import ./keyboard :as kb :fresh true)
(import ./vector-math :as v :fresh true)
(import ./theme)
(import ./fonts)
(import ./input :as i)
(import ./collision :prefix "")
(import ./render_new_gap_buffer :prefix "")
(import ./new_gap_buffer :prefix "")
(import bounded-queue :as queue)

(setdyn :freja/ns "freja/frp")

(var mouse nil)
(var chars nil)
(var keyboard nil)
(var frame-queue nil)
(var rerender nil)
(var out nil)
(var screen-size @{})

(var delay-left @{})

(defn handle-keys
  [dt]
  (var k (jay/get-char-pressed))

  (while (not= 0 k)
    (queue/push chars @[:char k])
    (set k (jay/get-char-pressed)))

  # must release keys before...
  (loop [k :in kb/possible-keys]
    (when (jay/key-released? k)
      (queue/push keyboard @[:key-release k])))

  # ...checking for held keys
  (loop [[k dl] :pairs state/keys-down
         # might just have been released
         :when (not (jay/key-released? k))
         :let [left ((update state/keys-down k - dt) k)]]
    (when (<= left 0)
      (queue/push keyboard @[:key-repeat k])))

  (loop [k :in kb/possible-keys]
    (when (jay/key-pressed? k)
      (queue/push keyboard @[:key-down k]))))

(varfn handle-scroll
  []
  (let [move (jay/get-mouse-wheel-move)]
    (when (not= move 0)
      (queue/push mouse @[:scroll (* move 30) (jay/get-mouse-position)]))))

(def callbacks @{:event/changed false})

(varfn handle-resize
  []
  (when (jay/window-resized?)
    (-> screen-size
        (e/put! :screen/width (jay/get-screen-width))
        (e/put! :screen/height (jay/get-screen-height)))))

(defn push-callback!
  [ev cb]
  (e/update! callbacks ev (fn [chan]
                            (default chan (queue/new 1))
                            (queue/push chan cb)
                            chan)))

(defn handle-callbacks
  [callbacks]
  (loop [[ev cbs] :pairs callbacks
         :when (not= ev :event/changed)]
    (e/pull-all cbs [apply]))

  (loop [k :in (keys callbacks)]
    (put callbacks k nil)))

(def mouse-data (i/new-mouse-data))

(varfn handle-mouse
  [mouse-data]
  (def pos (jay/get-mouse-position))
  (def [x y] pos)

  (put mouse-data :just-double-clicked false)
  (put mouse-data :just-triple-clicked false)

  (when (jay/mouse-button-released? 0)
    (put mouse-data :just-down nil)
    (put mouse-data :recently-double-clicked nil)
    (put mouse-data :recently-triple-clicked nil)
    (put mouse-data :up-pos [x y])

    (queue/push mouse @[:release (jay/get-mouse-position)]))

  (when (jay/mouse-button-pressed? 0)
    (when (and (mouse-data :down-time2)
               # max time to pass for triple click
               (> 0.4 (- (jay/get-time) (mouse-data :down-time2)))
               # max distance to travel for triple click
               (> 200 (v/dist-sqr pos (mouse-data :down-pos))))
      (put mouse-data :just-triple-clicked true)
      (put mouse-data :recently-triple-clicked true))

    (when (and (mouse-data :down-time)
               # max time to pass for double click
               (> 0.25 (- (jay/get-time) (mouse-data :down-time)))
               # max distance to travel for double click
               (> 100 (v/dist-sqr pos (mouse-data :down-pos))))
      (put mouse-data :just-double-clicked true)
      (put mouse-data :recently-double-clicked true)
      (put mouse-data :down-time2 (jay/get-time))))

  (cond (mouse-data :just-triple-clicked)
    (queue/push mouse @[:triple-click (jay/get-mouse-position)])

    (and (mouse-data :just-double-clicked)
         (not (jay/key-down? :left-shift))
         (not (jay/key-down? :right-shift)))
    (queue/push mouse @[:double-click (jay/get-mouse-position)])

    (or (mouse-data :recently-double-clicked)
        (mouse-data :recently-triple-clicked))
    nil # don't start selecting until mouse is released again

    (jay/mouse-button-down? 0)
    (do
      (put mouse-data :down-time (jay/get-time))

      (if (= nil (mouse-data :just-down))
        (do (put mouse-data :just-down true)
          (put mouse-data :last-pos pos)
          (put mouse-data :down-pos pos)
          (queue/push mouse @[:press (jay/get-mouse-position)]))
        (do (put mouse-data :just-down false)
          (unless (= pos (mouse-data :last-pos))
            (put mouse-data :last-pos pos)
            (queue/push mouse @[:drag (jay/get-mouse-position)])))))

    # no mouse button down
    (not= pos (mouse-data :last-pos))
    (do (put mouse-data :last-pos pos)
      (queue/push mouse @[:mouse-move (jay/get-mouse-position)]))))

(def deps @{})

(varfn render-deps
  [dt]
  (loop [d :in (deps :draws)]
    (d)))

(def finally
  @{frame-queue [render-deps]})

(defn handle-key-events
  [ev]
  (match ev
    [:key-release k]
    (put state/keys-down k nil)

    [:key-repeat k]
    (put state/keys-down k i/repeat-delay)

    [:key-down k]
    (put state/keys-down k i/initial-delay)))

(varfn init-chans
  []
  (print "initing chans")
  (set mouse (queue/new 100))

  (set chars (queue/new 100))
  (set keyboard (queue/new 100))
  (set frame-queue (queue/new 1))
  (set rerender (queue/new 1))
  (set out (queue/new 100))
  (set state/eval-results (queue/new 100))

  (def dependencies
    @{mouse @[]
      keyboard @[handle-key-events |(:on-event (state/focus :focus) $)]
      chars @[|(:on-event (state/focus :focus) $)]
      state/focus @[]
      callbacks @[handle-callbacks]
      out @[|(with-dyns [:out stdout]
               (print $))]})

  (def draws @[])

  (merge-into deps @{:deps dependencies
                     :draws draws
                     :finally finally}))

(varfn trigger
  [dt]
  (handle-keys dt)
  (handle-scroll)
  (handle-resize)

  (queue/push frame-queue @[:dt dt])

  (handle-mouse mouse-data)

  (e/pull-deps (deps :deps) (deps :finally)))

(defn subscribe-first!
  "Take an event emitter (e.g. a queue)
and a callback (e.g. single arity function).
Creates a regular subscription."
  [emitter cb]
  (unless (find |(= $ cb) (get-in deps [:deps emitter] []))
    (update-in deps [:deps emitter] (fn [$] @[cb ;(or $ [])]))))


(defn subscribe!
  "Take an event emitter (e.g. a queue)
and a callback (e.g. single arity function).
Creates a regular subscription."
  [emitter cb]
  (unless (find |(= $ cb) (get-in deps [:deps emitter] []))
    (update-in deps [:deps emitter] |(array/push (or $ @[]) cb))
    :ok))

(defn unsubscribe!
  "Take an event emitter (e.g. a queue)
and a callback (e.g. single arity function).
Removes a regular subscription."
  [emitter cb]
  (update-in deps [:deps emitter]
             (fn [subs] (filter |(not= $ cb) subs)))
  :ok)

(defn subscribe-finally!
  "Take an event emitter (e.g. a queue)
and a callback (e.g. single arity function).
Creates a finally subscription."
  [emitter cb]
  (unless (find |(= $ cb) (get-in deps [:finally emitter] []))
    (update-in deps [:finally emitter] |(array/push (or $ @[]) cb))
    :ok))


(defn unsubscribe-finally!
  "Take an event emitter (e.g. a queue)
and a callback (e.g. single arity function).
Removes a finFally subscription."
  [emitter cb]
  (update-in deps [:finally emitter]
             (fn [subs] (filter |(not= $ cb) subs)))
  :ok)
