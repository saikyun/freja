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
  (case (length v)
    2
    (let [[x y] v]
      (math/sqrt (+ (math/pow x 2)
                    (math/pow y 2))))

    3
    (let [[x y z] v]
      (math/sqrt (+ (math/pow x 2)
                    (math/pow y 2)
                    (math/pow z 2))))

    (error "mag only works on v2 / v3")))

(varfn mag-sqr
  [v]
  (case (length v)
    2
    (let [[x y] v]
      (+ (math/pow x 2)
         (math/pow y 2)))

    3
    (let [[x y z] v]
      (+ (math/pow x 2)
         (math/pow y 2)
         (math/pow z 2)))

    (error "mag-sqr only works on v2 / v3")))

(varfn dist-sqr
  [v1 v2]
  (math/abs
    (mag-sqr (v- v1 v2))))

(varfn dist
  [v1 v2]
  (math/abs
    (mag (v- v1 v2))))

(varfn normalize
  [v]
  (def m (mag v))
  (if (> m 0)
    (seq [p :in v]
      (/ p m))
    (seq [_ :in v]
      0)))
