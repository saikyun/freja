(import ./new_gap_buffer :prefix "")
(import ./theme :prefix "")
(import bounded-queue :as queue)

(setdyn :freja/ns "freja/state")

(var quit false)

(var gb-binds nil)

(var quit-hook nil)

(def freja-dir @"")

(var initial-file nil)

(def open-files
  @{})

(def focus @{})

(def keys-down @{})

(def out @"")
(def err @"")

(def editor-state @{})

(var user-env (make-env))

##### event queues

(def mouse (queue/new 100))
(def chars (queue/new 100))
(def keyboard (queue/new 100))
(def frame-events (queue/new 1))
(def rerender (queue/new 1))
(def out-events (queue/new 100))
(def eval-results (queue/new 100))
(def callbacks @{:event/changed false})
(def screen-size @{})

(def subscriptions @{})

##### handle different extenions

(def editor-components
  @{})

(def editor-state-creators
  @{})

(defn ext->editor
  [ext &opt data]
  (default ext (do (print "no ext provided, defaulting to .janet")
                 ".janet"))
  (def compo (get editor-components ext))
  (default compo (do (print "no component found for " ext ", defaulting to .janet")
                   (editor-components ".janet")))
  (def state-creator (get editor-state-creators ext))
  (default state-creator (do (print "no state-creator found for " ext ", defaulting to .janet")
                           (editor-state-creators ".janet")))
  [(compo data) (state-creator data)])

(defn add-ext-handling
  [ext component creator]
  (put editor-components ext component)
  (put editor-state-creators ext creator))

(defn push-buffer-stack
  [o]
  (def new-stack (-> (filter |(not= o $) (editor-state :stack))
                     (array/push o)))
  (-> editor-state
      (put :stack new-stack)
      (put :event/changed true)))

(defn remove-buffer-stack
  [o]
  (def new-stack (filter |(not= o $) (editor-state :stack)))
  (-> editor-state
      (put :stack new-stack)
      (put :event/changed true)))

(defn focus!
  ``
  Sets global focus to x.
  ``
  [x]
  (-> focus
      (put :last-focus (focus :focus))
      (put :focus x)
      (put :event/changed true)))
