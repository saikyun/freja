(import ./defonce :prefix "")
(import freja/frp)

(import freja/assets :as a)

(import freja-layout/sizing/definite :as def-siz)
(import freja-layout/sizing/relative :as rel-siz)
(import freja-layout/compile-hiccup :as ch)
(import freja-layout/jaylib-tags :as jt)

(import spork/test)

(defonce render-tree @{})

(var children-on-event nil)

(use profiling/profile)

(defn elem-on-event
  [e ev]
  # traverse children first
  # will return true if the event is taken
  #(pp (e :tag))
  #(pp (e :left))
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

  (loop [c :in cs
         :let [{:width w
                :height h
                :left x
                :top y} c]
         :until taken]

    #(with-dyns [:offset-x (+ (dyn :offset-x))
    #            :offset-y (+ (dyn :offset-y))]
    (set taken (elem-on-event c ev))) #)

  #

  taken)

(defn handle-ev
  [tree ev]
  (with-dyns [:offset-x 0
              :offset-y 0]
    (when (elem-on-event tree ev)
      (frp/push-callback! ev (fn [])))))

(defn compile-tree
  [hiccup props &keys {:max-width max-width
                       :max-height max-height
                       :tags tags
                       :text/font text/font
                       :text/size text/size
                       :old-root old-root}]

  (put props :compilation/changed true)

  (with-dyns [:text/font text/font
              :text/size text/size
              :text/get-font a/font]
    (print "compiling tree...")
    (def root #(test/timeit
      (ch/compile [hiccup props]
                  :tags tags
                  :element old-root)
      #)
)

    #(print "sizing tree...")
    (def root-with-sizes
      #(test/timeit
      (-> root
          (def-siz/set-definite-sizes max-width max-height)
          (rel-siz/set-relative-size max-width max-height))
      #)
)

    (put props :compilation/changed false)

    (print "hiccup nil?" (nil? root-with-sizes))

    root-with-sizes)

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
           (when (= :text-area (self :name))
             (p :render
                (with-dyns [:text/get-font a/font]
                  ((self :render)
                    (self :root))))))
   :on-event (fn [self ev]
               (p :on-event
                  (try
                    (match ev
                      @{:screen/width w
                        :screen/height h}
                      (do
                        (put self :max-width w)
                        (put self :max-height h)

                        (put self :root
                             (compile-tree
                               (self :hiccup)
                               (self :props)
                               :tags (self :tags)
                               :max-width (self :max-width)
                               :max-height (self :max-height)
                               :text/font (self :text/font)
                               :text/size (self :text/size)
                               :old-root (self :root))))

                      [:dt dt]
                      (:draw self dt)

                      '(table? ev)
                      (do # (print "compiling tree!")
                        (put self :props ev)
                        (put self :root
                             (compile-tree
                               (self :hiccup)
                               ev
                               :tags (self :tags)
                               :max-width (self :max-width)
                               :max-height (self :max-height)
                               :text/font (self :text/font)
                               :text/size (self :text/size)
                               :old-root (self :root))))

                      (p :handle-ev (handle-ev (self :root) ev)))

                    ([err fib]
                      (print "Error during event:")
                      (pp ev)
                      #(print "Hiccup: ")
                      #(pp ((self :hiccup) (self :props)))
                      #(print "Full tree:")
                      #(pp (self :root))
                      #(if (self :root)
                      #  (do
                      #    (print "Tree: ")
                      #    (ch/print-tree (self :root)))
                      #  (print "(self :root) is nil"))
                      (debug/stacktrace fib err)
                      (when (self :remove-layer-on-error)
                        (print "Removing layer: " (self :name))
                        (remove-layer (self :name) (self :props)))))))})

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

  (print "Adding hiccup layer: " name)

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
  (put render-tree :render |(do
                              #(when (= name :text-area)
                              #  (print "rendering hiccup"))
                              (render $)))

  (default max-width (frp/screen-size :screen/width))
  (put render-tree :max-width max-width)

  (default max-height (frp/screen-size :screen/height))
  (put render-tree :max-height max-height)

  (default tags jt/tags)
  (put render-tree :tags tags)

  (put render-tree :text/font text/font)
  (put render-tree :text/size text/size)

  (merge-into
    render-tree
    default-hiccup-renderer)

  (put props :event/changed true)

  (frp/subscribe! props render-tree)
  (frp/subscribe-finally! frp/frame-chan render-tree)
  (frp/subscribe! frp/mouse render-tree)
  (frp/subscribe! frp/screen-size render-tree)

  render-tree)
