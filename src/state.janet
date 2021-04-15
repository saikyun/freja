(def focus-checks @[])

(def updates @[])

(def draws @[])

(def fs @[])

(defn remove-f
  [f]
  (-?>> (find-index |(= $ f) fs)
        (array/remove fs)))

(defn add-f
  [f]
  (array/push fs f))
