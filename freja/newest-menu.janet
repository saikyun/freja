(import freja-layout/sizing/relative :as rs)
(use freja-layout/put-many)

(import freja/hiccup :as h)
(import freja/events :as e)
(import freja/frp)
(import freja/state)
(import freja/input :as i)
(import freja/new_gap_buffer :as gb)

(use freja/defonce)
(use freja-jaylib)

(setdyn :pretty-format "%.40M")

(defonce my-props @{})
(defonce state @{})

(put state :on-event (fn [self {:focus f}]
                       (when (get (f :gb) :open-file)
                         (put self :focused-text-area (f :gb)))))

(def label-color 0xffffffee)
(def hotkey-color 0xffffffbb)
(def damp-color 0xffffff88)
(def highlight-color 0xffffffee)
(def bar-bg 0x2D2D2Dff)
(def dropdown-bg 0x3E3E3Eff)

(def kws {:control "Ctrl"})

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

  (default hotkey (i/get-hotkey ((state :focused-text-area) :binds) f))
  (assert hotkey (string "no hotkey for " f))

  [:row {}
   [:align {:horizontal :left
            :weight 1}
    [:padding {:right 40}
     [:clickable {:on-click (fn [_]
                              (e/put! my-props :open-menu nil)
                              (f (state :focused-text-area)))}
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
    @{:render (fn [{:width w :height h} parent-x parent-y]
                (draw-rectangle 0 0 w (inc h) 0xffffff22))
      :relative-sizing rs/block-sizing
      :children []
      :props {}}]

   [menu-row
    {:f i/search2
     :label "Search"}]])

(defn hiccup
  [props & children]
  [:event-handler {:on-event
                   (fn [self [ev-kind]]
                     (when (my-props :open-menu)
                       (print "wtf?")
                       (when (= ev-kind :release)
                         (e/put! my-props :open-menu nil))))}

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

(defn init
  []
  (h/new-layer :menu hiccup
               my-props
               :remove-layer-on-error true)

  (frp/subscribe! state/focus state))

#
# this will only be true when running load-file inside freja
(when ((curenv) :freja/loading-file)
  (print "reiniting :)")
  (init))
