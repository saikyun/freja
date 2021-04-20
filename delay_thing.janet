(def click-queue (ev/chan 4))

(defn ev/check
  [chan]
  (when (pos? (ev/count chan))
    (ev/take chan)))

(defn ev/push
  [chan v]
  (when (ev/full chan)
    (ev/take chan)) ## throw away old values
  (ev/give chan v))

(def dumb-queue @[])

(defn take
  [queue]
  (array/pop queue))

(defn push
  [queue v]
  (array/insert queue 0 v))

(varfn draw-frame
  [dt]
  (when (mouse-button-pressed? 0)
    (def pos (get-mouse-position))
    (ev/go (coro (ev/sleep 1)
             (ev/push click-queue
                      (fn [] (print "delayed!")
                        (pp pos)))
             (push dumb-queue
                      (fn [] (print "dumb delayed!")
                        (pp pos))))))


  (loop [f :iterate (take dumb-queue)]
    (f))

  (loop [f :iterate (ev/check click-queue)]
    (f)))

(comment
  (ev/give click-queue (get-mouse-position))

  (print "clicks: ")
  (pp (check click-queue)))

(comment
  (defn delay
    [que]
    (ev/sleep 1)
    (print "hello " que)
    10)

  # (delay)  ### this would sleep current ("main") fiber
  # (resume (coro (delay))) ## this too

  (ev/go (fiber/new delay) 10)

  (let [f (fiber/new (fn [a]
                       (def b (yield a))
                       (yield b)))]
    (print (resume f 20))
    (print (resume f 50))))

(comment
  (doc resume)
  (doc ev/go)
  (doc coro)
  #
)
