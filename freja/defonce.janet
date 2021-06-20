(defmacro defonce
  "Define a value once."
  [name & more]
  (when (nil? (dyn name))
    ~(def ,name ,;more)))