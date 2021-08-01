(import freja-layout/jaylib-tags :as jt)
(import freja-layout/sizing/relative :as rs)
(import freja-layout/compile-hiccup :as ch)
(use freja-layout/put-many)

(import freja/hiccup :as h)
(import freja/events :as e)
(import freja/frp)
(import freja/input :as i)
(import freja/new_gap_buffer :as gb)

(use ./defonce)
(use jaylib)

(setdyn :pretty-format "%.40M")

(defonce my-props @{})

(def label-color 0xffffffee)
(def hotkey-color 0xffffffbb)
(def damp-color 0xffffff88)
(def highlight-color 0xffffffee)
(def bar-bg 0x2D2D2Dff)
(def dropdown-bg 0x3E3E3Eff)

(def kws {:control "Ctrl"})

(defn event-handler
  ``
Useful for handling any kind of event, even if it is not
happening inside of the layout bounding box.
``
  [props & _]
  (-> (dyn :element)
      (put-many
        :on-event (props :on-event)
        :relative-sizing rs/flow-sizing
        :props props)))

(defn kw->string
  [kw]
  (get kws
       kw
       (let [s (string kw)]
         (if (one? (length s))
           (string/ascii-upper s)
           s))))

(defn hotkey->string
  [hk]
  (string/join (map kw->string hk) "+"))

(defn menu-row
  [{:f f
    :label label
    :hotkey hotkey}]

  (default hotkey (i/get-hotkey ((frp/text-area :gb) :binds) f))
  (assert hotkey (string "no hotkey for " f))

  [:row {}
   [:align {:horizontal :left
            :weight 1}
    [:padding {:right 40}
     [:clickable {:on-click (fn [_]
                              (e/put! my-props :open-menu nil)
                              (f (frp/text-area :gb)))}
      [:text {:color label-color
              :size 22
              :text label}]]]]

   [:align {:horizontal :right
            :weight 1}
    [:text {:color hotkey-color
            :size 22
            :text (hotkey->string hotkey)}]]])

(defn file-menu
  [props]
  [:shrink {}
   [menu-row
    {:f i/open-file
     :label "Open"}]
   [menu-row
    {:f i/save-file
     :label "Save"}]
   [menu-row
    {:f i/quit
     :label "Quit"}]])

(defn edit-menu
  [props]
  [:shrink {}
   [menu-row
    {:f i/undo!2
     :label "Undo"}]
   [menu-row
    {:f i/redo!
     :label "Redo"}]
   [menu-row
    {:f i/cut!
     :label "Cut"}]
   [menu-row
    {:f gb/copy
     :label "Copy"}]
   [menu-row
    {:f i/paste!
     :label "Paste"}]

   [:padding {:all 8}
    @{:render (fn [{:width w :height h}]
                (draw-rectangle 0 0 w (inc h) 0xffffff22))
      :relative-sizing rs/block-sizing
      :children []
      :props {}}]

   [menu-row
    {:f i/search2
     :label "Search"}]])

(defn hiccup
  [props & children]
  [event-handler {:on-event
                  (fn [self [ev-kind]]
                    (when (= ev-kind :release)
                      (e/put! my-props :open-menu nil)))}

   [:padding {:left 0 :top 0}
    [:background {:color bar-bg}
     [:padding {:all 8 :top 4 :bottom 4}
      [:block {}
       [:row {}
        [:padding {:right 8}
         [:clickable {:on-click (fn [_]
                                  (e/put! props :open-menu :file))}
          [:text {:color (if (= (props :open-menu) :file)
                           highlight-color
                           damp-color)
                  :size 22
                  :text "File"}]]]

        [:clickable {:on-click (fn [_]
                                 (e/put! props :open-menu :edit))}
         [:text {:color (if (= (props :open-menu) :edit)
                          highlight-color
                          damp-color)
                 :size 22
                 :text "Edit"}]]]]]]

    (when-let [om (props :open-menu)]
      (case om
        :file
        [:background {:color dropdown-bg}
         [:padding {:all 8
                    :top 3}
          [file-menu props]]]
        :edit
        [:block {}
         [:padding {:right 8}
          [:text {:color 0x00000000 :size 22 :text "File"}]]
         [:background {:color dropdown-bg}
          [:padding {:all 8
                     :top 3}
           [edit-menu props]]]]))]])
(comment
  (defn hiccup
    [props & children]
    [:padding {:top 40 :left 600}
     [:background {:color :red}
      [:shrink {}
       [:row {}
        [:flow {:weight 1}
         [:align {:horizontal :left}
          [:padding {:right 100}
           "Open"]]]

        [:flow {:weight 1}
         "wat"]]]]]))
(var c nil)

(def b 10)

b

(defn init
  []
  (set c (h/new-layer :test-layer2
                      hiccup
                      my-props
                      :render jt/render
                      :tags jt/tags
                      :text/font "Poppins"
                      :text/size 22
                      :max-width (get-screen-width)
                      :max-height (get-screen-height))))

# this will only be true when running load-file inside freja
(when ((curenv) :freja/loading-file)
  (print "reiniting :)")
  (init))

(comment
  # the below can be used to print the render tree
  (import freja-layout/compile-hiccup :as ch :fresh true)
  (import freja-layout/sizing/definite :as ds :fresh true)
  (import freja-layout/sizing/relative :as rs :fresh true)

  (let [el (ch/compile [hiccup my-props]
                       :tags jt/tags)
        el (ds/set-definite-sizes el 800 600)
        el (rs/set-relative-size el 800 600)]
    (ch/print-tree el))
  #
)
