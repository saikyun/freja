(import ./../src/events :prefix "")
(import ./../src/extra_channel :prefix "")

### ------------ initialization starts here ----------

## the functions a user might create

(def state @{:button1 (fn [self] (print "button 1 render: " (self :button1-state)))
             :button2 (fn [self] (print "button 2 render: " (self :button2-state)))
             :changed false})

# renders the state
(defn render-f
  [state]
  (print)
  (print "Rendering a very pretty scene with buttons ^_^")
  (:button1 state)
  (:button2 state))

# a button that will take all clicks
(defn button1
  [ev]
  (def [kind [x y]] ev)
  (case kind
    :down (push-callback! ev (fn []
                               (put! state :button1-state
                                     (string "clicked button1 at - x: " x " y: " y))))))

# a button that will take all clicks except when x is 20
(defn button2
  [ev]
  (def [kind [x y]] ev)
  (unless (= x 20)
    (case kind
      :down (push-callback! ev (fn []
                                 (put! state :button2-state
                                       (string "clicked button2 at - x: " x " y: " y)))))))

## -- these are dependencies
## eg button1 and button2 depend on mouse
## so every time a mouse event occurs, button1 and button2
## are called
(var dependencies @{callbacks @[handle-callbacks]
                    mouse @[button1 button2 # pp
                            ]
                    ## the order of the functions matter
                    ## in this case, button1 will be called before button2
                    ## this means that button2s callback will "replace" button1s, if they both trigger
                    ## you can put any function here, like pp :)
                    state @[render-q]})

## the finally part will only be run once
## it is assumed that the functions here won't
## push any new events
(var finally @{render-q @[render-f]})

# this turns on recording for all dependencies
(record-all dependencies)

# just for fun, keep track how many loops of
# event checking is done
(var iterations 0)

### ------------ imaginary mouse clicks --------------

# we add some events
(ev/push! mouse @[:down [30 100]])
(ev/push! mouse @[:down [30 100]])
(ev/push! mouse @[:down [20 110]])


### ------------ execution starts here ---------------

(print "-- state from beginning")
(pp state)
(print)

# as long as dependencies have changed (are `fresh?`)
# keep looping through them and tell dependees
# that changes have happened (`pull-all`)
(while (some fresh? (keys dependencies))
  (print "iteration " (++ iterations))
  (loop [[pullable pullers] :pairs dependencies]
    (pull-all pullable pullers)))

# then finally do the "rendering" (printing)
(loop [[pullable pullers] :pairs finally]
  (pull-all pullable pullers))

# some change has happened to the state
(print)
(print "-- state has been mutated")
(pp state)
(print)



### this could be called `reset-history`
# it recreates all state (tables)
# and puts all events back onto the pullables
(loop [[pullable pullers] :pairs dependencies]
  (when-let [hi-i (find-index |(and (table? $) ($ :history)) pullers)]
    (def history (pullers hi-i))
    (array/remove pullers hi-i)
    (case (type pullable)
      :core/channel (pull-all (history :history) [pullable])
      :table        (do (loop [k :in (keys pullable)]
                          (put pullable k nil))
                      (merge-into pullable (history :history)))
      (error (string "Strange" (type pullable))))
    (array/push pullers history)))

(print "-- state reset from history")
(pp state)
(print)

## then we replay, it's just the same as before
(while (some fresh? (keys dependencies))
  (print "iteration " (++ iterations))
  (loop [[pullable pullers] :pairs dependencies]
    (pull-all pullable pullers)))

(loop [[pullable pullers] :pairs finally]
  (pull-all pullable pullers))

(print)
(print "-- state mutated again")
(pp state)
(print)

(comment
  # functions for looking inside the channels (pullables)
  (tracev (ev/vs mouse))
  )

