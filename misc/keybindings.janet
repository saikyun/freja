(def global-keys @{})

(def modifiers [:caps-lock :shift])

(def check-modifiers
  {:caps-lock |(key-down? :caps-lock)
   :shift |(or (key-down? :left-shift)
               (key-down? :right-shift))})

(defn set-key
  [kmap ks f]
  (var key nil)
  (var mods @[])

  # this ensures mods are ordered the same way
  (loop [m :in modifiers
         k :in ks]
    (if (= k m)
      (array/push mods k)
      (set key k)))

  (array/push mods key)
  (put-in global-keys mods f))

(def global-set-key (partial set-key global-keys))

(defn hotkey-triggered
  [kmap]
  (def mods (seq [m :in modifiers
                  :when ((check-modifiers m))]
              m))

  (var ret-f nil)
  (loop [[k f] :pairs (or (get-in kmap mods) [])
         :when (function? f) # on partial mod-combination, f will be a table
         :when (key-down? k)]
    (set ret-f f)
    (break))

  ret-f)

(varfn draw-frame
  [dt]
  (when-let [f (hotkey-triggered global-keys)]
    (f gb-data)))


(comment

  (global-set-key [:caps-lock :shift :a] |(select-all $))

  (global-set-key [:shift :a] |(print "hej " $))
  (pp global-keys)

  #
)
