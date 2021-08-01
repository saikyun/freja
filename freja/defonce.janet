(defmacro defonce
  "Define a value once.
Useful when repling and not wanting to replace a definition."
  [name & more]
  (when (nil? (dyn name))
    ~(def ,name ,;more)))

(defmacro varonce
  "Var a value once.
Useful when repling and not wanting to replace a definition."
  [name & more]
  (when (nil? (dyn name))
    ~(var ,name ,;more)))