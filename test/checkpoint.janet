(import ../freja/main)
(import ../freja/frp)
(import ../freja/events :as e)
(import ../freja/default-hotkeys :as dh)
(import ../freja/state)
(import ../freja/new_gap_buffer :as gb)
(import ../freja/checkpoint :as c)

(math/seedrandom (os/time))

# silly way to get random path
(def path (string "random-test-path" (math/floor (* 10000 (math/random))) ".janet"))
(def content @"hello!")

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
  @[;(press :left-control
            ;(press :o))
    ;(chars path)
    ;(press :enter)
    ;(chars content)
    ;(press :left-control
            ;(press :s))])

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
      (let [top-buf (last (state/editor-state :stack))
            path2 (get-in top-buf [1 :editor :gb :path])
            checkpoints (c/list-checkpoints path2)
            cp-path (last (in (last checkpoints) 1))]
        (os/rm path2)
        (if (and (deep= content
                        (tracev (gb/content (get-in top-buf [1 :editor :gb]))))
                 (deep= content
                        (slurp cp-path)))
          (do
            (os/rm cp-path)
            (print "test successful\n------------------------------")

            (os/exit 0))
          (do
            (os/rm cp-path)
            (print "!!! test failed !!!\n------------------------------")
            (os/exit 1)))))))


(main/main nil nil "--no-init")

(frp/subscribe!
  frp/keyboard
  (fn [v]
    (with-dyns [:out stdout]
      (print "frp key: ")
      (pp v))))

(run-commands)
