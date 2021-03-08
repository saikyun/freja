# this comment block has tests
(comment

  # 1) a comment block test has two pieces:
  #
  # 1) the form
  # 2) the expected value comment
  #
  (+ 1 1)
  # => 2

  # 2) another example

  # the following form is executed, as it has no expected value, but its
  # effects will remain valid for subsequent portions
  (def my-value 1)

  # note the use of `my-value` here
  (struct :a my-value :b 2)
  # => {:a 1 :b 2}

  # 3. below shows one way to express nested content more readably
  (let [i 2]
    (deep=
      #
      (struct :a (/ i i)
              :b [i (+ i 1) (+ i 3) (+ i 5)]
              :c {:x (math/pow 2 3)})
      #
      {:a 1
       :b [2 3 5 7]
       :c {:x 8}}))
  # => true

  # 4. one way to express expected values involving tuples, is to use
  #    square bracket tuples...
  (tuple :a :b)
  # => [:a :b]

  # 5. alternatively, quote ordinary tuples in the expected value comment
  (tuple :x :y :z)
  # => '(:x :y :z)

  )

# this comment block does not have tests because there are
# no expected values expressed as line comments
(comment

  (print "hi")

  (each x (range 3)
    (print x))

  )
