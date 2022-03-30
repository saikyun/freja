(import ../freja/main)
(import ../freja/state)
(import ../freja/new_gap_buffer :as gb)
(import ../freja/checkpoint :as c)
(import ./util/replay-events :as r)

(math/seedrandom (os/time))

# silly way to get random path
(def path (string "random-test-path" (math/floor (* 10000 (math/random))) ".janet"))
(def content @"hello!")

(main/main nil nil "--no-init")

(var commands
  @[;(r/press :left-control
              ;(press :o))
    ;(r/chars path)
    ;(r/press :enter)
    ;(r/chars content)
    ;(r/press :left-control
              ;(r/press :s))])

(r/run-commands
  (fn []
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
