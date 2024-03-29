(use ./defonce)

(import ./state)
(import ./event/subscribe :as s)
(import ./event/callback)
(import ./assets :as a)

(import freja-layout/sizing/definite :as def-siz)
(import freja-layout/sizing/relative :as rel-siz)
(import freja-layout/compile-hiccup :as ch)
(import freja-layout/jaylib-tags :as jt)

(defonce render-tree @{})

(var children-on-event nil)

(use profiling/profile)

(defn elem-on-event
  [e ev]
  # traverse children first
  # will return true if the event is taken
  (with-dyns [:offset-x (+ (dyn :offset-x)
                           (get e :left 0)
                           (get-in e [:offset 3] 0))
              :offset-y (+ (dyn :offset-y)
                           (get e :top 0)
                           (get-in e [:offset 0] 0))]
    (if
      (children-on-event e ev)
      true

      (when (e :on-event)
        (:on-event e ev)))))

(varfn children-on-event
  [{:children cs
    :content-width content-width} ev]
  (var taken false)

  (var x 0)
  (var y 0)
  (var row-h 0)

  (def max-h (dyn :event-max-h))

  (loop [c :in cs
         :let [{:width w
                :height h
                :left x
                :top y} c]
         :until (or taken (> y max-h))]
    (set taken (elem-on-event c ev)))

  taken)


(defn handle-ev
  [tree ev]
  # only run events if no one else has already taken the event
  (unless (state/callbacks ev)
    # offset is the top left corner of each element
    (with-dyns [:offset-x 0
                :offset-y 0
                :event-max-h (tree :height)]
      (when (elem-on-event tree ev)
        (callback/put! ev (fn []))))))

(defn compile-tree
  [hiccup props &keys {:max-width max-width
                       :max-height max-height
                       :tags tags
                       :text/font text/font
                       :text/size text/size
                       :old-root old-root}]
  (let [to-init @[]]
    (put props :compilation/changed true)

    (with-dyns [:text/font text/font
                :text/size text/size
                :text/get-font a/font]
      (def root #(test/timeit
        (ch/compile [hiccup props]
                    :tags tags
                    :element old-root
                    :to-init to-init))

      (def root-with-sizes
        (-> root
            (def-siz/set-definite-sizes max-width max-height)
            (rel-siz/set-relative-size max-width max-height)))

      (put props :compilation/changed false)

      (ch/init-all to-init)

      root-with-sizes))

  #
)


# table with all layers that have names
# if a new layer is created with a name
# it is added to named-layers
# if it already exists in named-layers,
# instead the layer to be added replaces
# the layer already existing
(defonce named-layers @{})

(defn remove-layer
  [name props]
  (when-let [l (named-layers name)]
    (put l :on-event (fn [& _])))
  (put named-layers name nil))

(def default-hiccup-renderer
  {:draw (fn [self dt]
           (with-dyns [:text/get-font a/font]
             ((self :render)
               (self :root))))
   :compile (defn compile
              [self props]
              (compile-tree
                (self :hiccup)
                props
                :tags (self :tags)
                :max-width (self :max-width)
                :max-height (self :max-height)
                :text/font (self :text/font)
                :text/size (self :text/size)
                :old-root (self :root)))

   :on-event (defnp hiccup-on-event [self ev]
               (try
                 (match ev
                   {:screen/width w
                    :screen/height h}
                   (do
                     (put self :max-width w)
                     (put self :max-height h)

                     (put self :root (:compile self (self :props))))

                   {:frame/delta-time dt}
                   (:draw self dt)

                   # new props
                   {:compilation/changed _}
                   (-> self
                       (put :props ev)
                       (put :root (:compile self ev)))

                   (handle-ev (self :root) ev))

                 ([err fib]
                   (eprint "Error during event:")
                   (eprintf "%P" ev)

                   (propagate err fib)

                   (when (self :remove-layer-on-error)
                     (printf "Removing layer: %s" (self :name))
                     (remove-layer (self :name) (self :props))))))})

(defn new-layer
  [name
   hiccup
   props
   &keys {:render render
          :max-width max-width
          :max-height max-height
          :tags tags
          :text/font text/font
          :text/size text/size
          :remove-layer-on-error remove-layer-on-error}]

  (def render-tree (or (named-layers name)
                       (let [c @{}]
                         (put named-layers name c)
                         c)))

  # reset the component
  (loop [k :keys render-tree]
    (put render-tree k nil))

  (put render-tree :hiccup hiccup)

  (put render-tree :remove-layer-on-error remove-layer-on-error)

  (put render-tree :name name)
  (put render-tree :props props)

  (default render jt/render)
  (put render-tree :render |(render $ 0 0))

  (default max-width (state/screen-size :screen/width))
  (put render-tree :max-width max-width)

  (default max-height (state/screen-size :screen/height))
  (put render-tree :max-height max-height)

  (default tags jt/tags)
  (put render-tree :tags tags)

  (put render-tree :text/font text/font)
  (put render-tree :text/size text/size)

  (merge-into
    render-tree
    default-hiccup-renderer)

  #(put props :event/changed true)

  (put render-tree :root (:compile render-tree props))

  (s/subscribe! props render-tree)
  (s/subscribe-finally! state/frame-events render-tree)
  (s/subscribe-first! state/mouse render-tree)
  (s/subscribe! state/screen-size render-tree)

  render-tree)
