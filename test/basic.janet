(import ../freja/main)
(import ../freja/state)
(import ../freja/new_gap_buffer :as gb)
(import ./util/replay-events :as replay)

(var commands
  @[@{:key/down :left-control}
    @{:key/down :a}
    @{:key/release :a}
    @{:key/release :left-control}
    @{:key/char :a}])

(main/main)

(replay/run-commands
  commands
  (fn []
    (with-dyns [:out stdout]
      (if (deep= @"a" (gb/content (get-in state/editor-state [:stack 0 1 :editor :gb])))
        (do
          (print "test successful\n------------------------------")
          (os/exit 0))
        (do (print "!!! test failed !!!\n------------------------------")
          (os/exit 1))))))
