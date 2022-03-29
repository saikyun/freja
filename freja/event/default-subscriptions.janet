(import bounded-queue :as queue)
(import ../state)
(import ./callback)
(import ./jaylib-to-events :as jaylib->events)

(defn init
  []
  (let [subscriptions
        @{state/mouse @[]
          state/keyboard @[jaylib->events/handle-key-events |(:on-event (state/focus :focus) $)]
          state/chars @[|(:on-event (state/focus :focus) $)]
          state/focus @[]
          state/callbacks @[callback/handle]
          state/out-events @[|(with-dyns [:out stdout]
                                (print $))]}

        # subscribers to `frame-events will
        # be invoked whenever a new frame should be rendered
        # freja uses this mostly by having `freja/hiccup`
        # subscribe to `state/frame-events`
        #
        # this empty list won't actually do anything
        finally @{state/frame-events @[]}]

    # the subscriptions are used in `freja/main/internal-frame`
    # when `events/pull-and-push` is called
    (merge-into state/subscriptions @{:regular subscriptions
                                      :finally finally})))
