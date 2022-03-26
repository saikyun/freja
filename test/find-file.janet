(import ../freja/main)
(import ../freja/frp)
(import bounded-queue :as queue)
(import ../freja/default-hotkeys :as dh)
(import ../freja/state)
(import ../freja/new_gap_buffer :as gb)
(import spork/path)

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
       ;(press :p))
    ;(chars "popp li")
    ;(press :down)
    ;(press :enter)])

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
      (if (= (tracev (path/join "freja" "fonts" "Poppins-Regular_LICENSE"))
             (tracev (((last (state/editor-state :stack)) 1) :freja/label)))
        (do
          (print "test successful\n------------------------------")
          (os/exit 0))
        (do (print "!!! test failed !!!\n------------------------------")
          (os/exit 1))))))

#(frp/subscribe! frp/keyboard pp)

(main/main nil nil "--no-init")

(run-commands)
