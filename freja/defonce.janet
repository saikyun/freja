(defmacro defonce
  "Define a value once.
Useful when repling and not wanting to replace a definition."
  [name & more]
  (when (nil? (dyn name))
    ~(upscope
       (def ,name ,;more)
       (put (dyn ',name) :defonce true)
       ,name)))

(defmacro varonce
  "Var a value once.
Useful when repling and not wanting to replace a definition."
  [name & more]
  (when (nil? (dyn name))
    ~(upscope
       (var ,name ,;more)
       (put (dyn ',name) :defonce true)
       ,name)))
