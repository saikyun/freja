(import ../freja/main)
(import ../freja/frp)
(import bounded-queue :as queue)
(import ../freja/default-hotkeys :as dh)
(import ../freja/state)
(import ../freja/new_gap_buffer :as gb)

(defn press
  [k & body]
  [@[:key-down k]
   ;body
   @[:key-release k]])

(defn chars
  [s]
  (map (fn [c]
         @[:char (keyword (string/from-bytes c))]) s))

(var commands
  @[;(press
       :left-control
       ;(press :p))])

(defn run-commands
  [& _]
  (print "running commands")
  (ev/spawn
    (ev/sleep 0.2)
    (loop [c :in commands]
      (ev/sleep 0.000001) # this means we will get ~1 input per frame
      (print "pushing c: ")
      (pp c)
      (if (= :char (first c))
        (queue/push frp/chars @[;c])
        (queue/push frp/keyboard @[;c])))
    (ev/sleep 0.00001)
    (with-dyns [:out stdout]
      (print "just checks if not crashing")
      (print "test successful\n------------------------------")
      (os/exit 0))))

(main/main)

(frp/subscribe!
  frp/keyboard
  (fn [v]
    (with-dyns [:out stdout]
      (print "frp key: ")
      (pp v))))

(run-commands)
