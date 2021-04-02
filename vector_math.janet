(varfn v-op
  [op v1 v2]
  (if (number? v2)
    (seq [v :in v1]
      (op v v2))
    (seq [i :range [0 (min (length v1) (length v2))]]
      (op (v1 i) (v2 i)))))

(var v+ (partial v-op +))
(var v- (partial v-op -))
(var v* (partial v-op *))

(varfn mag
  [v]
  (assert (= (length v) 2)
          "mag only works on v2")

  (def [x y] v)

  (math/sqrt (+ (math/pow x 2)
                (math/pow y 2))))

(varfn normalize
  [v]
  (def m (mag v))
  (if (> m 0)
    (seq [p :in v]
      (/ p m))
    [0 0]))