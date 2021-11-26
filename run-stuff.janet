(import freja/frp)
(import freja/events :as e)
(import freja/default-hotkeys :as dh)
(import freja/state)

(var ln nil)

(var commands #@[]
  @[@[:char :a] @[:key-down :backspace]

    ;(seq [_ :range [0 50]] @[:key-repeat :down])

    |(set ln (last (get-in state/editor-state [:left-state :editor :gb :line-numbers])))

    @[:key-down :page-up]
    @[:key-down :page-up]

    ;(seq [_ :range [0 50]] @[:key-repeat :down])

    @[:key-repeat :left-control] @[:key-down :m]])

(var recording false)

(defn push-command [c] (array/push commands c))

(defn toggle-record
  [_]
  (if recording
    (do
      (frp/unsubscribe! frp/keyboard push-command)
      # last one is hotkey
      (set commands (array/slice commands 1 (length commands))))
    (frp/subscribe! frp/keyboard push-command))
  (set recording (not recording))
  (print "recording is on? " recording))

(defn run-commands
  [& _]
  (do
    (print "running commands")
    (ev/spawn
      (ev/sleep 0.2)
      (loop [c :in (array/slice commands 0 (dec (length commands)))]
        (ev/sleep 0.000001)
        (if (function? c)
          (c)
          (e/push! frp/keyboard @[;c])))

      (pp (get-in state/editor-state [:left-state :editor :gb :lines]))
      (let [l (last (get-in state/editor-state [:left-state :editor :gb :line-numbers]))]
        (print l)

        (print l " eq to " ln "? ")

        (if (= l ln)
          (print "success!")
          (print "#### ERROR !!!!"))

        (if (= l ln)
          (os/exit 1)
          (os/exit 1)))

      #      (set state/quit true)
)))

(var recording false)

(defn push-command [c] (array/push commands c))

(defn toggle-record
  [_]
  (if recording
    (do
      (frp/unsubscribe! frp/keyboard push-command)
      # last one is hotkey
      (set commands (array/slice commands 1 (length commands))))
    (frp/subscribe! frp/keyboard push-command))
  (set recording (not recording))
  (print "recording is on? " recording))

(dh/global-set-key [:control :m] toggle-record)
(dh/global-set-key [:control :r] run-commands)

(run-commands)
