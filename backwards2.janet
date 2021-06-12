(varfn in-rec?
  [[px py] [x y w h]]
  (and
    (>= px x)
    (<= px (+ x w))
    (>= py y)
    (<= py (+ y h))))
