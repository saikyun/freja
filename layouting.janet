(defn v+!
  ``
Destructivly adds two vectors of size 2 together.
Mutates the first argument.
``
  [v0 v1]
  (update v0 0 + (v1 0))
  (update v0 1 + (v1 1)))

(defn size->px
  ``
Takes a container-size which should be an array/tuple with
absolute pixel sizes, e.g. `[100 30]'.
`grid-size` is either in absolute pixel values,
or in percentage form, e.g. `[[10 :%] [20 :%]]`.
This would mean the grid item should be 10% the width of
the container, and 20% of the height.
``
  [container-size grid-size]
  (seq [i :range [0 (length grid-size)]
        :let [s (grid-size i)]]
    (match (tracev s)
      [n :%] (* n 0.01 (container-size i))
      [n :px] n
      n n)))

(defn get-sizes
  ````
Takes a `dom` table/struct, which has this format:
```
{:size size          # e.g. [20 40] for w/h in pixels
                     # can be nil

 :children children  # array/tuple of children
                     # each child should be a dom table/struct

 :padding padding    # e.g. [10  20    30     40]
                     #       top right bottom left padding
                     # to remember, think of a clock starting at the top

 :grid grid          # array/tuple of grid sizes for children
                     # e.g. [[100 30] [20% 10%]] would mean
                     # first child has 100px width, 30px height
                     # second child has 20% width, 10% height
                     # when using %, it is based on the size of the parent
}
```
````
  [dom &keys {:container container
              :grid-size grid-size}]
  (def {:size size
        :children children
        :padding padding
        :grid grid}
    dom)

  (def me @{})

  ## try to determine my size
  (put me :size-px
       ## do I have an explicit size?
       (cond size
         (seq [i :range [0 (length size)]
               :let [s (size i)]]
           (match s
             #[n :%] (* n 0.01 ((container :size-px) i))
             [n :px] n
             n n))

         ## or maybe a grid size?
         grid-size))

  (when children
    (put me :children
         (seq [i :range [0 (length children)]
               :let [c (children i)
                     grid-size (tracev (-?>> (get grid i)
                                             (size->px (tracev size))))]]
           (get-sizes c
                      :grid-size (tracev grid-size)
                      :container me)))
    #    (put me :children (map |(get-sizes me $) children))
)

  (default padding [0 0 0 0])

  # padding positions
  # 0 = top
  # 1 = right
  # 2 = bottom
  # 3 = left
  # same as css. like a clock starting at 12
  (def padding-size
    @[(+ (padding 1) (padding 3))
      (+ (padding 0) (padding 2))])

  (unless (me :size-px)
    (put me :size-px
         (v+!
           (reduce (fn [a c]
                     (v+! a (c :size-px)))
                   @[0 0]
                   (or (me :children) []))
           padding-size)))

  me)

(defn traverse
  ``
Applies f to each dom node.
Useful for debugging.
``
  [f tree]
  (f tree)
  (when-let [cs (tree :children)]
    (each c cs (traverse f c))))

(comment
  (def tree
    (get-sizes {:size [800 600]
                :grid [[[10 :%] [10 :%]]
                       [[20 :%] [20 :%]]]
                :children
                [{:children
                  [{:children
                    [{:padding [1000 1000 1000 1000]
                      :children
                      [{:size [30 20]}]}]}]}
                 {}]}))
  (pp tree)
  @{:children @[@{:children @[@{:children @[@{:children @[@{:size-px @[30 20]}] :size-px @[2030 2020]}] :size-px @[2030 2020]}] :size-px @[80 60]} @{:size-px @[160 120]}] :size-px @[800 600]}

  (traverse |(pp ($ :size-px)) tree)
  @[800 600]
  @[80 60]
  @[2030 2020]
  @[2030 2020]
  @[30 20]
  @[160 120]

  #
)

(import freja/frp)

(defn draw
  [[_ dt]]
  (print "123321123"))

(defonce render-it @{})

(merge-into
  render-it
  @{:on-event (fn [_ ev] (draw ev))})

(frp/subscribe-finally! frp/frame-chan render-it)
