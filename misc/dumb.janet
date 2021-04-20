(def callbacks @{})

(defn push-cb
  [k f]
  (update callbacks k |(array/push (or @[] $) f)))

(def objs [@{:hitbox @[810 30 100 100]
             :render |(draw-rectangle-rec ($ :hitbox) :red)
             :update |(when (and (mouse-button-down? 0)
                                 (in-rec? (get-mouse-position)
                                          ($ :hitbox)))
                        (push-cb :click ($ :f)))
             :f |(print "red!")}
           @{:hitbox @[860 0 100 100]
             :render |(draw-rectangle-rec ($ :hitbox) :blue)
             :update |(when (and (mouse-button-down? 0)
                                 (in-rec? (get-mouse-position)
                                          ($ :hitbox)))
                        (push-cb :click ($ :f)))
             :f |(print "blue!")}])

(varfn draw-frame
  [dt]
  (loop [b :in objs]
    (:update b))

  (loop [[_ cbs] :pairs callbacks
         :when (not (empty? cbs))]
    # when multiple callbacks with same key triggered
    # we only call the last one
    ((last cbs))
    (array/clear cbs))

  (loop [b :in objs]
    (:render b)))
