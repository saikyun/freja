(import ../freja/main)
(import ../freja/default-hotkeys :as dh)
(import ../freja/state)
(import ../freja/new_gap_buffer :as gb)
(import spork/path)


(var commands
  @[;(press
       :left-control
       ;(press :p))
    ;(chars "popp li")
    ;(press :down)
    ;(press :enter)])

(main/main nil nil "--no-init")

(run-commands
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
