# functions to not block the fiber when interacting with channels
(defn pop
  "Like ev/take but non-blocking, instead returns `nil` if the channel is empty."
  [chan]
  (when (pos? (ev/count chan))
    (ev/take chan)))

(defn push!
  "Like ev/give, but if the channel is full, throw away the oldest value."
  [chan v]
  (when (ev/full chan)
    (ev/take chan)) ## throw away old values
  (ev/give chan v))

(defn vs
  "Returns the values in a channel."
  [chan]
  (def vs @[])
  
  # empty the queue
  (loop [v :iterate (pop chan)]
    (array/push vs v))
  
  # then put them back again
  (loop [v :in vs]
    (push! chan v))
  
  vs)

