(import bounded-queue :as queue)
(import ../state)

# a push-pull system
# events are pushed to queues
# then things can pull from the queues

# we want to be able to pull.
# multiple things should be able to pull from it,
# essentially splitting/copying the value

(defn pull
  [pullable pullers]
  (when-let [v (cond
                 # is a queue -- kind of wishing for a way to define custom types
                 (pullable :items)
                 (queue/pop pullable)

                 # regular table
                 :else
                 (when (pullable :event/changed)
                   (put pullable :event/changed false)))]

    (loop [puller :in pullers]
      (try
        (case (type puller)
          :function (puller v)
          :table (cond
                   (puller :items)
                   (queue/push puller v)

                   :else
                   (:on-event puller v))
          (error (string "Pulling not implemented for " (type puller))))
        ([err fib]
          (queue/push state/eval-results (if (and (dictionary? err) (err :error))
                                           err
                                           {:error err
                                            :fiber fib
                                            :msg (string/format ``
%s
event:
%p 
subscriber:
%p
``
                                                                err
                                                                (if (dictionary? v)
                                                                  (string/format "dictionary with keys: %p" (keys v))
                                                                  v)
                                                                (if (dictionary? puller)
                                                                  (string/format "dictionary with keys: %p" (keys puller))
                                                                  puller))
                                            :cause [v puller]})))))
    v) # if there was a value, we return it
)

(defn pull-all
  [pullable pullers]
  (while
    (pull pullable pullers)
    nil))

(defn put!
  ```
  Same as `put` but also puts `:event/changed` to true.
  ```
  [state k v]
  (-> state
      (put k v)
      (put :event/changed true)))

(defn update!
  ```
  Same as `update` but also puts `:event/changed` to true.
  ```
  [state k f & args]
  (-> state
      (update k f ;args)
      (put :event/changed true)))

(defn fresh?
  [pullable]
  (cond
    (pullable :items) (not (queue/empty? pullable))
    :else (pullable :event/changed)))

(defn call-subscribers
  ``
  Loops through all subscriptions, pulls from the emitters.
  When events are pulled, they are pushed onto the subscribers.
  ``
  [subscriptions &opt finally]
  # as long as dependencies have changed (are `fresh?`)
  # keep looping through them and tell dependees
  # that changes have happened (`pull-all`)
  # this is important since laters subscriptions might add
  # events to earlier emitters
  (while (some fresh? (keys subscriptions))
    (loop [[pullable pullers] :pairs subscriptions]
      (pull-all pullable pullers)))

  # then when all is done, run the things in `finally`
  (loop [[pullable pullers] :pairs (or finally {})]
    (pull-all pullable pullers)))


### functions used to add / remove subscriptions after init-subscriptions has been called

(defn subscribe-first!
  "Take an event emitter (e.g. a queue)
and a callback (e.g. single arity function).
Creates a regular subscription."
  [emitter cb]
  (unless (find |(= $ cb) (get-in state/subscriptions [:regular emitter] []))
    (update-in state/subscriptions [:regular emitter] (fn [$] @[cb ;(or $ [])]))))


(defn subscribe!
  "Take an event emitter (e.g. a queue)
and a callback (e.g. single arity function).
Creates a regular subscription."
  [emitter cb]
  (unless (find |(= $ cb) (get-in state/subscriptions [:regular emitter] []))
    (update-in state/subscriptions [:regular emitter] |(array/push (or $ @[]) cb))
    :ok))

(defn unsubscribe!
  "Take an event emitter (e.g. a queue)
and a callback (e.g. single arity function).
Removes a regular subscription."
  [emitter cb]
  (update-in state/subscriptions [:regular emitter]
             (fn [subs] (filter |(not= $ cb) subs)))
  :ok)

(defn subscribe-finally!
  "Take an event emitter (e.g. a queue)
and a callback (e.g. single arity function).
Creates a finally subscription."
  [emitter cb]
  (unless (find |(= $ cb) (get-in state/subscriptions [:finally emitter] []))
    (update-in state/subscriptions [:finally emitter] |(array/push (or $ @[]) cb))
    :ok))


(defn unsubscribe-finally!
  "Take an event emitter (e.g. a queue)
and a callback (e.g. single arity function).
Removes a finFally subscription."
  [emitter cb]
  (update-in state/subscriptions [:finally emitter]
             (fn [subs] (filter |(not= $ cb) subs)))
  :ok)
