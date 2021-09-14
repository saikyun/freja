(use freja-jaylib)

(import freja/frp)
(import freja/state)

(comment
  (do
    (var cp 0)
    (frp/subscribe! (get-in state/editor-state [:left-state :editor])
                    (fn [state]
                      (unless (= cp (get-in state [:gb :caret-pos]))
                        (play-sound (weak (rand-pos weak)))
                        (set cp (get-in state [:gb :caret-pos])))))
    :ok)
  #
)


# (init-audio-device)

(def weak (->> [(load-wave "misc/sound/svag1.wav")
                (load-wave "misc/sound/svag2.wav")
                (load-wave "misc/sound/svag3.wav")
                (load-wave "misc/sound/svag4.wav")]
               (map load-sound-from-wave)))

(def strong (->> [(load-wave "misc/sound/stark1.wav")
                  (load-wave "misc/sound/stark2.wav")
                  (load-wave "misc/sound/stark3.wav")
                  (load-wave "misc/sound/stark4.wav")]
                 (map load-sound-from-wave)))

(var last 0)

(defn rand-pos [xs] (math/floor (* (length xs) (math/random))))

(def active weak)
#(def active strong)
(var new (rand-pos active))

(while (= last new)
  (set new (rand-pos active)))

(set last new)

(play-sound (active new))
