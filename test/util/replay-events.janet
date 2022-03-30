(import bounded-queue :as queue)
(import ../../freja/state)

(defn press
  [k & body]
  [@[:key-down k]
   ;body
   @[:key-release k]])

(defn chars
  [s]
  (map (fn [c]
         @[:char (keyword (string/from-bytes c))]) s))

(defn run-commands
  [commands done-f]
  (ev/spawn
    (ev/sleep 0.2)
    (loop [c :in commands]
      (ev/sleep 0.000001) # this means we will get ~1 input per frame
      (if (= :char (first c))
        (queue/push state/chars @[;c])
        (queue/push state/keyboard @[;c])))
    (ev/sleep 0.00001)
    (done-f)))