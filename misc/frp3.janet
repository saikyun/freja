(use jaylib)

(import ./../top_bar2 :as tb)

(def state-ref @{:ch (ev/chan 1) :data @[]})
(def screen-size-ref @{:ch (ev/chan 1) :data [(get-screen-width) (get-screen-height)]})
(def rt-ref @{:ch (ev/chan 1) :data (load-render-texture ;(screen-size-ref :data))})
(def render-queue-ref @{:ch (ev/chan 1) :data nil})
(def clicks-ref @{:ch (ev/chan 10) :data [-1 -1]})
(def new-frame-ref @{:ch (ev/chan 1) :data nil})
(def callbacks-ref @{:ch (ev/chan 10) :data @[]})

(defn ev/check
  [chan]
  (when (pos? (ev/count chan))
    (ev/take chan)))

(defn ev/push
  [chan v]
  (when (ev/full chan)
    (ev/take chan)) ## throw away old values
  (ev/give chan v))

(defn swap!
  [ref f & vs]
  (let [new-data (f (ref :data) ;vs)]
    (ev/push (ref :ch) new-data)
    (put ref :data new-data)))

(defn split
  [ch target-chs]
  {:fib
   (ev/call
     (fn []
       (forever
         (let [v (ev/take ch)]
           (loop [c :in target-chs]
             (ev/give c v))))))
   :chs target-chs})

(def rendering @{:listen [state-ref screen-size-ref rt-ref]

                 :id :rendering

                 :update (fn [self state screen-size rt]
                           (begin-texture-mode rt)

                           (clear-background :blank)

                           (draw-rectangle 800 0 (get-screen-width) (get-screen-height) (colors :background))

                           # we draw twice in order to draw on both back and front buffer
                           (loop [o :in state
                                  :when (o :render)]
                             (:render o))
                           (end-drawing)

                           (begin-drawing)
                           (draw-rectangle 800 0 (get-screen-width) (get-screen-height) (colors :background))

                           (loop [o :in state
                                  :when (o :render)]
                             (:render o))

                           (end-texture-mode)

                           (swap! render-queue-ref
                                  (fn [_] |(draw-texture-pro
                                             (get-render-texture rt)
                                             [0 0 (get-screen-width) (- (get-screen-height))]
                                             [0 0 (get-screen-width) (get-screen-height)]
                                             [0 0]
                                             0
                                             :white))))})

(defn listen
  [ref o]
  (unless (get-in o [:listeners ref])
    (def ch (ev/chan 10))
    (update o :listeners |(put (or $ @{}) ref ch))

    (if-let [{:fib fib :chs chs} (ref :split)]
      (do #(ev/cancel fib "k")
        (put ref :split (split (ref :ch) (array/push chs ch))))
      (put ref :split (split (ref :ch) @[ch])))
    (update o :last-res |(let [arr (or $ (array 1))]
                           (put arr (dec (length (o :listeners))) (ref :data))))))

(defn add-listeners
  [o]
  (loop [l :in (o :listen)]
    (listen l o))
  o)

(defn run
  [state]
  (loop [o :in state
         :let [{:listeners listeners
                :listen listen
                :last-res last-res} o]]
    (var any-change false)

    (loop [i :range [0 (length listen)]
           :let [l (listen i)
                 ls (listeners l)]]
      (when-let [v (ev/check ls)]
        (set any-change true)
        (put last-res i v)))

    (when any-change
      (:update o ;(o :last-res)))))

(var draw-layout nil)
(var trigger-update-layout nil)

(defn pad-left
  [padding]
  (case (length padding)
    1 (first padding)
    2 (padding 1)
    4 (padding 3)))

(defn pad-top
  [padding]
  (case (length padding)
    1 (first padding)
    2 (padding 0)
    4 (padding 0)))

(def button {:render (fn [self {:size size :padding padding :color color} children]
                       (draw-rectangle
                         ((dyn :anchor) 0)
                         ((dyn :anchor) 1)
                         (get size 0 0)
                         (get size 1 0)
                         color)

                       (with-dyns [:anchor (array ;(dyn :anchor))]
                         (update (dyn :anchor) 0 + (pad-left padding))
                         (update (dyn :anchor) 1 + (pad-top padding))
                         (when children
                           (map draw-layout children)))

                       (if (= (dyn :layout) :horizonal)
                         (update (dyn :anchor) 0 + (get size 0 0))
                         (update (dyn :anchor) 1 + (get size 1 0))))
             :update (fn [self {:size size :padding padding :on-click on-click} children]
                       (when (and (mouse-button-down? 0)
                                  (in-rec?
                                    (get-mouse-position)
                                    [((dyn :anchor) 0)
                                     ((dyn :anchor) 1)
                                     (get size 0 0)
                                     (get size 1 0)]))
                         (swap! callbacks-ref array/push on-click))

                       (with-dyns [:anchor (array ;(dyn :anchor))]
                         (update (dyn :anchor) 0 + (pad-left padding))
                         (update (dyn :anchor) 1 + (pad-top padding))
                         (when children
                           (map trigger-update-layout children)))

                       (if (= (dyn :layout) :horizonal)
                         (update (dyn :anchor) 0 + (get size 0 0))
                         (update (dyn :anchor) 1 + (get size 1 0))))})

(varfn draw-layout
  [c]
  (with-dyns [:anchor (or (dyn :anchor) @[0 0])
              :layout :horizontal]
    (def [f props] c)
    (def children (array/slice c 2))
    (:render f props children)))

(varfn trigger-update-layout
  [c]
  (with-dyns [:anchor (or (dyn :anchor) @[0 0])
              :layout :horizontal]
    (def [f props] c)
    (def children (array/slice c 2))
    (:update f props children)))

(defn noop [_ _ _] nil)

(def p {:render (fn [self props [text]]
                  (draw-text-ex tb/font text (dyn :anchor) tb/font-size tb/spacing :black)
                  (print "draw text " text))
        :update noop})

(def state
  (->> @[### buttons
         @{:listen [clicks-ref state-ref]
           :hitbox @[810 0 100 70]
           :render (fn [self]
                     (draw-rectangle-rec (self :hitbox) :red))
           :update (fn [self click state]
                     (when (in-rec? click (self :hitbox))
                       (swap! callbacks-ref array/push |(print (length state)))))}

         @{:listen [clicks-ref]
           :hitbox @[860 20 100 70]
           :render (fn [self]
                     (draw-rectangle-rec (self :hitbox) :blue))
           :update (fn [self click]
                     (when (in-rec? click (self :hitbox))
                       (print "pushing")
                       (swap! callbacks-ref array/push |(print "blue"))))}

         @{:listen [clicks-ref]
           :layout [button {:size [100 60]
                            :padding [20 30]
                            :on-click (fn [] (print "Hi sogaiu :D"))
                            :color :green}
                    [p {} "Hello 123"]]
           :render (fn [self] (draw-layout (self :layout)))
           :update (fn [self click]
                     (trigger-update-layout (self :layout)))}

         ### rendering
         rendering

         @{:listen [new-frame-ref render-queue-ref]
           :update (fn [self _ rq]
                     (rq))}

         ### resolving callbacks
         @{:listen [callbacks-ref]
           :update (fn [self cbs]
                     (when-let [cb (last cbs)]
                       (cb)
                       (array/clear cbs)))}]
       (map add-listeners)))

(defn add-obj
  [state-ref o]
  (swap! state-ref
         array/push
         (add-listeners o)))

(comment
  # eval the following sexp to get another "button"
  (add-obj
    state-ref
    @{:hitbox @[810 100 100 70]
      :render (fn [self]
                (print "green")
                (draw-rectangle-rec (self :hitbox) :green))

      :listen [clicks-ref state-ref]
      :update (fn [self click state]
                (when (in-rec? click (self :hitbox))
                  (swap! callbacks-ref
                         array/push
                         |(print "green" (length state)))))})
  #
)

# initialize state
(swap! state-ref (fn [_] state))

(varfn draw-frame
  [dt]
  (swap! new-frame-ref (fn [_] []))

  (when (mouse-button-pressed? 0)
    (swap! clicks-ref (fn [_] (get-mouse-position))))

  (run (state-ref :data)))
