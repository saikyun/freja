(import ./defonce :prefix "")
(import ./frp)
(use jaylib)

(defonce change-window-title @{})

(defn init
  []
  (merge-into
    change-window-title
    @{:last-path nil

      :on-event (fn [self ev]
                  (let [p (get-in ev [:gb :path])]
                    (print "text-area changed (path: " p ")")
                    (unless (= p (self :last-path))
                      (set-window-title (string p " - Freja"))
                      (put self :last-path p))))})

  (frp/subscribe! frp/text-area change-window-title))
