(def state-ref @{:ch (ev/chan 1) :data @[]})
(def clicks-ref @{:ch (ev/chan 10) :data [-1 -1]})
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

(defn listen
  [ref o]
  (unless (get-in o [:listeners ref])
    (def ch (ev/chan 10))
    (update o :listeners |(put (or $ @{}) ref ch))

    (if-let [{:fib fib :chs chs} (ref :split)]
      (do #(ev/cancel fib "k")
        (put ref :split (split (ref :ch) (array/push chs ch))))
      (put ref :split (split (ref :ch) @[ch]))))

  (update o :last-res |(let [arr (or $ (array 1))]
                         (put arr (dec (length arr)) (ref :data)))))

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
      (apply (o :update) o (o :last-res)))))

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

         ### rendering
         @{:listen [state-ref]

           :update (fn [self state]
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
                       (:render o)))}

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
  (when (mouse-button-pressed? 0)
    (swap! clicks-ref (fn [_] (get-mouse-position))))

  (run (state-ref :data)))
