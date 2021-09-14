(import ./new_gap_buffer :prefix "")
(import ./theme :prefix "")

(setdyn :freja/ns "freja/state")

(var quit false)

(var quit-hook nil)

(def freja-dir @"")

(var initial-file nil)

(var eval-results nil)

(def focus @{})

(def out @"")
(def err @"")

(def editor-state @{})

(var user-env (make-env))

(defn ev/check
  [chan]
  (when (pos? (ev/count chan))
    (ev/take chan)))

(defn ev/push
  [chan v]
  (when (ev/full chan)
    (ev/take chan)) ## throw away old values
  (ev/give chan v))

(defn swap!
  [ref f & vs]
  (let [new-data (f (ref :data) ;vs)]
    (ev/push (ref :ch) new-data)
    (unless (ref :no-history)
      (put ref :data new-data))))
