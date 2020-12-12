(setdyn :pretty-format "%.20M")

(def state
  @{:x 0 :y 0})

(def rows
  [{:y 10
    :words ["hello" " " " " "mr cat"]
    :buffer @"hello  mr cat"
    :xs    [0 5 10 15 (comment etc)]}
   {:y 20
    :words ["hello" " " " " "mr cat"]
    :buffer @"hello  mr cat"
    :xs    [0 5 10 15 (comment etc)]}])

(defn binary-search
  [vs comp]
  (when (zero? (length vs))
    (break nil))
  
  (def i (math/floor (/ (length vs) 2)))
  (def v (vs i))
  
  (case (comp v)
    -1 (binary-search (array/slice vs 0 i) comp)
    0  i
    1  (-?> (binary-search (array/slice vs (inc i)) comp)
         (+ (inc i)))))

(defn binary-search-closest*
  [vs c offset]
  (if (zero? (length vs))
    offset
    (let [i (math/floor (/ (length vs) 2))
          v (vs i)]
      (case (c v)
        -1 (binary-search-closest*
             (array/slice vs 0 i)
             c
             offset)
        0  (+ offset i)
        1  (binary-search-closest*
             (array/slice vs (inc i))
             c
             (+ offset (inc i)))))))

(defn binary-search-closest
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
  (binary-search-closest* vs c 0))

(defn binary-search-closest3*
  [vs bottom top c]
  (if (>= bottom top)
    top
    (let [i (math/floor (/ (- top bottom) 2))
          v (vs (+ i bottom))]
      (case (c v)
        -1 (binary-search-closest3*
             vs
             bottom
             (+ bottom i)
             c)
        0  i
        1  (binary-search-closest3*
             vs
             (inc (+ bottom i)) 
             top
             c)))))

(defn binary-search-closest3
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
  (binary-search-closest3* vs 0 (length vs) c))

(defn binary-search-closest2
  [vs c]
  (var res nil)
  (var bottom 0)
  (var top (length vs))
  (while (nil? res)
    (if (>= bottom top)
      (set res top)
      (let [i (math/floor (/ (- top bottom) 2)) 
            v (vs (+ i bottom))]
        (case (c v)
          -1 (set top (+ bottom i))
          0  (set res (+ bottom i))
          1  (set bottom (inc (+ bottom i)))))))
  res)
