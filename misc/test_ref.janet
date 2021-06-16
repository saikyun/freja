(import ./frp3 :prefix "")
(import ./../freja/state :prefix "")


(def thing-ref @{:ch (ev/chan 5)})

(def l (-> @{:listen [thing-ref]
             :update (fn [self thing]
                       (pp self)
                       (print thing))}
           add-listeners))

(varfn trigger
  [dt]
  (run @[l]))

(comment
  
  (defn unlisten
    [ref o]
    (def ch (get-in l [:listeners thing-ref]))
    (update-in thing-ref [:split :chs] (fn [chs] (filter |(not= $ ch) chs)))
    (put-in l [:listeners thing-ref] nil)
    (update o :listen (fn [refs] (filter |(not= $ ref) refs)))
    (pp (ref :split)))
  
  (swap! thing-ref (fn [_] :hello12354))
  
  (unlisten-all l)
  )


:ok
