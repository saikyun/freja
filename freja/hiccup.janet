(import ./defonce :prefix "")
(import freja/frp)

(import freja/assets :as a)

(import freja-layout/sizing/definite :as def-siz)
(import freja-layout/sizing/relative :as rel-siz)
(import freja-layout/compile-hiccup :as ch)

(import spork/test)


# TODO: remove
(use jaylib)

(defonce render-tree @{})

(var children-on-event nil)

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
    #(print "compiling tree...")
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


(defn new-layer
  [name
   hiccup
   props
   &keys {:render render
          :max-width max-width
          :max-height max-height
          :tags tags
          :text/font text/font
          :text/size text/size}]

  (def render-tree (or (named-layers name)
                       (let [c @{}]
                         (put named-layers name c)
                         c)))

  # reset the component
  (loop [k :keys render-tree]
    (put render-tree k nil))

  (put render-tree :hiccup hiccup)

  (assert render "must provide a :render function to `new-layer`")
  (put render-tree :render render)

  (put render-tree :max-width max-width)
  (assert max-width "must provide :max-width")

  (put render-tree :max-height max-height)
  (assert max-height "must provide :max-height")

  (put render-tree :tags tags)
  (assert tags "must provide :tags")

  (merge-into
    render-tree
    @{:draw (fn [self dt]
    (with-dyns [:text/get-font a/font]
              ((self :render)
                (self :root))))
      :on-event (fn [self ev]
                  (match ev
                    [:dt dt]
                    (do

                      (when (window-resized?)
                        (put self :max-width (get-screen-width))

                        (put self :root
                             (compile-tree
                               (self :hiccup)
                               (self :props)
                               :tags tags
                               :max-width (self :max-width)
                               :max-height (self :max-height)
                               :text/font text/font
                               :text/size text/size
                               :old-root (self :root))))

                      (:draw self dt))

                    '(table? ev)
                    (do # (print "compiling tree!")
                      (put self :props ev)
                      (put self :root
                           (compile-tree
                             (self :hiccup)
                             ev
                             :tags tags
                             :max-width (self :max-width)
                             :max-height (self :max-height)
                             :text/font text/font
                             :text/size text/size
                             :old-root (self :root))))

                    (handle-ev (self :root) ev)))})

  (put props :event/changed true)

  (put-in frp/deps [:deps props] [render-tree])
  (frp/subscribe-finally! frp/frame-chan render-tree)
  (frp/subscribe! frp/mouse render-tree)

  render-tree)
