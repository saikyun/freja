(import freja/frp)
(import freja/events :as e)
(import freja/default-hotkeys :as dh)
(import freja/state)
(import freja/new_gap_buffer :as gb)

(var commands
  @[@[:key-down :left-control]
    @[:key-down :a]
    @[:key-release :a]
    @[:key-release :left-control]
    @[:char :a]])

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
        (e/push! frp/chars @[;c])
        (e/push! frp/keyboard @[;c])))
    (ev/sleep 0.00001)
    (with-dyns [:out stdout]
      (if (deep= @"a" (tracev (gb/content (get-in state/editor-state [:stack 0 1 :editor :gb]))))
        (do
          (print "test successful")
          (os/exit 0))
        (do (print "!!! test failed !!!")
          (os/exit 1))))))

(run-commands)
