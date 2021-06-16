(import ./freja/frp :as frp)
(import ./freja/state :as state)
(import jaylib :as jay)

(defonce component @{})

(frp/subscribe! frp/mouse component)
(frp/subscribe-finally! frp/frame-chan component)

(def font-size 24)
(def h 28)
(var looking-at @{:hello-there-fine-person 10
                  :y 20
                  :table @{:very-much-data :yes}})
(var looking-at gb-data)
(def stack @[])

(merge-into
  component
  {:x 800

   :draw
   (fn [self dt]
     (when (= self (state/focus123 :focus))
       (draw-rectangle-rec [790 30 1000 1000] 0x0000cc05))

     (draw-text "< Back" 800 40 font-size 0x000000ee)

     (let [ks (sort (keys looking-at))]
       (loop [i :range [0 (length looking-at)]
              :let [k (ks i)
                    v (looking-at k)
                    y (+ 80 (* i h))
                    xv (+ 820 (jay/measure-text (string k) font-size))
                    #
]]
         (draw-text (string k) 800 y font-size 0x000000ee)
         (draw-text (string v) xv y font-size 0x000000ee)))

     #
)

   :handle-click
   (fn [self [ev [mx my]]]
     (when (= ev :press)
       (if (and (>= my 40)
                (< my (+ 40 h)))
         (when-let [v (array/pop stack)]
           (set looking-at v))

         (let [ks (sort (keys looking-at))]
           (var change-to nil)
           (loop [i :range [0 (length looking-at)]
                  :let [k (ks i)
                        v (looking-at k)
                        y (+ 80 (* i h))
                        xv (+ 820 (jay/measure-text (string k) font-size))
                        #
]]
             (when (and (>= my y)
                        (< my (+ y h)))
               (when (table? v)
                 (set change-to v)))
             #
)
           (when change-to
             (array/push stack looking-at)
             (set looking-at change-to))))))

   :on-event
   (fn [self ev]
     (case (ev 0)
       :press
       (when
         (>= (get-in ev [1 0]) 800)
         (put state/focus123 :focus self)
         (:handle-click self ev))

       :key-down
       (case (ev 1)
         :a (update self :x + 10))

       :dt
       (:draw self (ev 1)))
     #
)})

(comment
  component
  gb-data)
