(import ./frp4 :prefix "")
(import ./../src/input :as i)

(def dependencies
  @{mouse @[button button2 text-area search-area file-open-area]
    keyboard @[|(:on-event (focus :focus) $)]
    chars @[|(:on-event (focus :focus) $)]
    focus @[caret]
    callbacks @[handle-callbacks]})

(put deps :deps dependencies)

(defn global-set-key
  [ks f]

  (defn f1
    [props]
(print "hehe")
    (case (length ks)
      1 (f props)
      2 (when (key-down? (first ks))
          (f props))))

  (put i/gb-binds
       (last ks)
       f1))


(global-set-key [:caps-lock (keyword ";")] save-file)
(global-set-key [:caps-lock :i] copy)
(global-set-key [:caps-lock :.] paste!)
(global-set-key [:caps-lock :b] cut!)
(global-set-key [:caps-lock :p] save-and-dofile)
(global-set-key [:caps-lock :a] select-all)
(global-set-key [:caps-lock :o] backward-word)

(comment

  (global-set-key [:caps-lock (keyword ";")] save-file)

  #
)
