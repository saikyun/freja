(import ./freja/events :as e :fresh true)

(var debug-data (new-gap-buffer))

(defn dumb-set
  "Creates a table that acts like a set, where the value is equal to the key."
  [& ks]
  (table ;(flatten (map (fn [k] [k k]) ks))))

(defn remove-keys
  [t ks]
  (def nt @{})
  (loop [[k v] :pairs t
         :when (not (ks k))]
    (put nt k v))
  nt)

(do (merge-into debug-data
                @{:size [400 :max]
                  :position [810 60]
                  :offset [10 0]
                  :dont-refocus true

                  :id :debug})
  :ok)

(def debug-o text-area)

(defn debug-f
  [area])


#
#
#
#
#
#
#



(def debug-area
  @{:id :debug

    :gb (merge-into
          debug-data
          @{:binds @{}

            :search
            (fn [props]
              (e/put! focus :focus search-area))

            :open-file
            open-file})

    :draw (fn [self]
            (replace-content
              debug-data
              (string/format "%.5m"
                             (remove-keys (debug-o :gb)
                                          (dumb-set
                                            :actions
                                            :binds
                                            :colors
                                            :conf
                                            :context
                                            :delim-ps
                                            :highlighting
                                            :sizes))))

            (rl-pop-matrix)

            #(end-texture-mode)

            (gb-pre-render (self :gb))

            #(begin-texture-mode (rt-ref :data))

            (rl-push-matrix)

            (rl-load-identity)

            #(rl-scalef 2 2 1)

            (gb-render-text (self :gb)))

    :on-event (fn [self ev]
                (text-area-on-event self ev))})


(defn init-debug
  []
  (def tc @{:font-path "./assets/fonts/Monaco.ttf"
            :size (* 14 1)
            :line-height 1.2
            :mult (/ 1 1)
            :glyphs default-glyphs
            :spacing 0.5})
  (set conf3 (load-font debug-data tc))
  (put debug-data :context data)
  (put debug-data :screen-scale [1 1])
  (put debug-data :colors colors))

(init-debug)


(def dependencies
  @{mouse @[text-area debug-area search-area file-open-area]
    keyboard @[#pp
               |(:on-event (focus :focus) $)]
    debug-o @[debug-f]
    chars @[|(:on-event (focus :focus) $)]
    focus @[|(print (($ :focus) :id)) caret]
    callbacks @[handle-callbacks]})

(put deps :deps dependencies)

(def draws @[|(:draw text-area)
             |(:draw debug-area)
             |(case (focus :focus)
                search-area (:draw search-area)
                file-open-area (:draw file-open-area))
             |(:draw caret)])

(put deps :draws draws)
