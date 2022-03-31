(import bounded-queue :as queue)
(import ../state)
(import ./subscribe :as s)

(defn put!
  ``
  Tells the callback handler that callback wants
  to be called due to the event `ev`.
  Only the last callback put this way will be called by `handle`.
  By default, this is used to make sure that only the topmost graphical element
  gets to do something with an event.
  ``
  [ev cb]
  (s/update! state/callbacks
             ev
             (fn [chan]
               # only the last callback will be called
               (default chan (queue/new 1))
               (queue/push chan cb)
               chan)))

(defn handle
  ``
  Calls the callbacks for each event.
  Then clears all stored callbacks.

  Is normally called once per frame.
  ``
  [callbacks]
  (loop [[ev cbs] :pairs state/callbacks
         :when (not= ev :event/changed)]
    (s/pull-all cbs [apply]))

  (table/clear callbacks))

