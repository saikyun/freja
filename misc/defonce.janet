(defmacro defonce
  "Define a value once."
  [name & more]
  (when (nil? (dyn name))
    ~(def ,name ,;more)))

(defonce cat @{:name "hello"})

(pp cat)

(put cat :name "bert")

(defonce cat @{:name "hello"})

(pp cat)
