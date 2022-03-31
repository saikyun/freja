(import ../freja/main)
(import ../freja/default-hotkeys :as dh)
(import ../freja/state)
(import ../freja/new_gap_buffer :as gb)
(import ./util/replay-events :as r)

(var commands
  @[;(r/press
       :left-control
       ;(r/press :p))])

(main/main)

(replay/run-commands
  commands
  (fn
    []
    (with-dyns [:out stdout]
      (print "test successful\n------------------------------")
      (os/exit 0))))
