# a push-pull system
# events are pushed to queues
# then things can pull from the queues

(import ./extra_channel :as ec)

# first some example queues
# we use ev/chan just because it's a queue
# we won't use the event handling of ev
(def mouse (ev/chan 10)) # mouse events, such as `[:down  [30  100]]`
#                                                  ^ kind  ^ x ^ y

(def tick (ev/chan 1)) # delta times, eg `10` (ms)
(def render-q (ev/chan 1)) # render calls, will just call the render function
#                              # when pulled. you can put any value here

# then we want to be able to pull
# multiple things should be able to pull from it
# essentially splitting the value

(defn pull
  [pullable pullers]
  (when-let [v (case (type pullable)
                 :core/channel (ec/pop pullable)
                 :table (when (pullable :event/changed)
                          (put pullable :event/changed false))
                 (error (string (type pullable) " is not a pullable.")))]
    (loop [puller :in pullers]
      (try
        (case (type puller)
          :function (puller v)
          :core/channel (ec/push! puller v)
          :table (:on-event puller v)
          (error (string "Pulling not implemented for " (type puller))))
        ([err fib]
          (debug/stacktrace fib err))))
    v) # if there was a value, we return it
)

(defn pull-all
  [pullable pullers]
  (while
    (pull pullable pullers)
    nil))

(print)
(ec/vs mouse)

(defn put!
  [state k v]
  (-> state
      (put k v)
      (put :event/changed true)))

(defn update!
  [state f & args]
  (-> state
      (update f ;args)
      (put :event/changed true)))

(defn record-all
  [pullables]
  (loop [[pullable pullers] :pairs pullables]
    (case (type pullable)
      :core/channel
      (array/push pullers
                  @{:history (ev/chan 10000)
                    :on-event (fn [self ev]
                                (update self :history ec/push! ev))})

      :table
      (array/push pullers
                  @{:history (freeze pullable)
                    :on-event (fn [self ev] nil)})))

  pullables)

(defn fresh?
  [pullable]
  (case (type pullable)
    :core/channel (pos? (ev/count pullable))
    :table (pullable :event/changed)))

(varfn pull-deps
  [deps &opt finally]
  # as long as dependencies have changed (are `fresh?`)
  # keep looping through them and tell dependees
  # that changes have happened (`pull-all`)
  (while (some fresh? (keys deps))
    (loop [[pullable pullers] :pairs deps]
      (pull-all pullable pullers)))

  # then when all is done, run the things in `finally`
  (loop [[pullable pullers] :pairs (or finally {})]
    (pull-all pullable pullers)))
