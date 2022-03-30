(import ../freja/main)
(import ../freja/default-hotkeys :as dh)
(import ../freja/state)
(import ../freja/new_gap_buffer :as gb)
(import ./util/replay-events :as replay)

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

(main/main)

(replay/run-commands
  commands
  (fn
    []
    (with-dyns [:out stdout]
      (print "test successful\n------------------------------")
      (os/exit 0))))
