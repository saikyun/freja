(import ../freja/main)
(import ../freja/default-hotkeys :as dh)
(import ../freja/state)
(import ../freja/new_gap_buffer :as gb)
(import spork/path)
(import ./util/replay-events :as r)

(var commands
  @[;(r/press
       :left-control
       ;(r/press :p))
    ;(r/chars "popp li")
    ;(r/press :down)
    ;(r/press :enter)])

(main/main nil nil "--no-init")

(r/run-commands
  commands
  (fn []
    (with-dyns [:out stdout]
      (if (= (tracev (path/join "freja" "fonts" "Poppins-Regular_LICENSE"))
             (tracev (((last (state/editor-state :stack)) 1) :freja/label)))
        (do
          (print "test successful\n------------------------------")
          (os/exit 0))
        (do (print "!!! test failed !!!\n------------------------------")
          (os/exit 1))))))
