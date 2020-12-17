(varfn binary-search-closest*
  [vs bottom top c]
  (if (>= bottom top)
    top
    (let [i (math/floor (/ (- top bottom) 2))
          v (vs (+ i bottom))]
      (case (c v)
        -1 (binary-search-closest*
             vs
             bottom
             (+ bottom i)
             c)
        0  (+ i bottom)
        1  (binary-search-closest*
             vs
             (inc (+ bottom i)) 
             top
             c)))))

(varfn binary-search-closest
  ``
  Binary searches sorted array `vs` using single arity function `c`.
  `c` is called on elements of `vs`, and is expected to return `-1`, `0` or `1` (like `compare`).
  Returns index of match.
  If an exact match can't be found, return the closest index, rounded upwards.
  
  (binary-search-closest [0 1 2] (partial compare 1)) #=> 1
  (binary-search-closest [0 1 2] (partial compare 2)) #=> 2
  (binary-search-closest [0 1 2] (partial compare 1.5)) #=> 2
  ``
  [vs c]
  (binary-search-closest* vs 0 (length vs) c))
