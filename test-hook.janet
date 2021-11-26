(def a1
  (* 2 3 4 5 6 7 8 9 10))

(import freja/new_gap_buffer :as gb)
(import freja/state)

# will contain the parsers for each line
(def cache @[])

(def gap-buffer (get-in state/editor-state [:left-state :editor :gb]))

(var buf "")

(def ast [@[] @{}])

(defn build-ast
  [])

(gb/add-hook
  gap-buffer
  :hooks/new-line
  :parse
  (fn [gb k line-i current-line]
    (when (< line-i 3)

      (set buf (string buf current-line))

      (let [parser (or (-?> (with-dyns [:out stdout]
                              (def v (get-in cache [(dec line-i) :parser]))
                              # (pp v)
                              # (tracev v)
                              v)
                            (parser/clone))
                       (parser/new))]
        (def curr @{:parser parser
                    :line-i line-i
                    :line (string current-line)
                    :forms @[]
                    :states @[]
                    :symbols @[]})

        (with-dyns [:out stdout]
          (print "lul: " current-line))
        (var curr-delim nil)

        (array/push (curr :states)
                    (parser/state parser))
        (def curr-stack @[])

        (put (ast 1) line-i @[])

        (var column 0)

        (loop [t :in (->> current-line
                          (string/replace "(" " ( ")
                          (string/replace ")" " ) ")
                          (string/split " ")
                          (filter |(not (empty? $))))]
          (parser/consume parser (string t " "))

          (def end (+ column (length t)))
          (array/push ((ast 1) line-i) [t
                                        column
                                        end
                                        (parser/state parser)])
          (set column end)

          #(array/push cache (parser/state parser))
          (def state (parser/state parser))
          (array/push curr-stack state)
          (comment unless (= curr-delim (state :delimiters))
                   # delimiters changed
                   (set curr-delim (state :delimiters))
                   (array/push (curr :forms) curr-delim)

                   (array/concat (curr :forms)
                                 ;(seq [f :in (state :frames)
                                        #a :in (f :args)
                                        :let [as (f :args)]
                                        :when as
                                        :when (not (empty? as))]
                                    (f :args))))

          #(comment
          (def vs (seq [v :iterate (parser/produce parser)]
                    (array/push (curr :forms) v)
                    (set curr-delim nil)
                    (array/clear curr-stack)
                    v))
          #
          #)

          (when-let [err (parser/error parser)]
            (print "error: " err)
            (print "current token: " t)
            (print "current line: " current-line)
            (pp (parser/state parser))
            (put curr :error err))
          #
)

        (array/push cache curr)))))

# to recalculate all cache, we can clear lines
(array/clear (gap-buffer :lines))
# and say that it was changed
(put gap-buffer :changed true)

(gb/add-hook
  gap-buffer
  :hooks/invalidate-cache
  :store-lines
  # line-i is the line index that is invalidated
  # that means all following lines will be recalculated,
  # so we remove those
  (fn [gb k line-i]
    #(print "removing from: " line-i " to " (length cache))
    (when (< line-i (length cache))
      (array/remove cache line-i (length cache)))))


(import freja/state)
(import freja/events :as e)
(use freja-jaylib)
(import ./freja/custom :as c)
(import freja/render_new_gap_buffer :as rgb)

(defn safe-slice
  [s start stop]
  (string/slice s start (min (length s) stop)))

(defn debug-string
  [v]
  (-> (string/format "%p" v)
      (safe-slice 0 1000)))

(defn show-keys
  [o ks]
  (string/join
    (seq [k :in ks
          :let [s (string/format "%p %p" k (get o k))]]
      (safe-slice s 0 1000))
    "\n"))

(e/put!
  state/editor-state :right
  (fn [{:left-state rs}]
    (def {:editor editor} rs)
    (def {:gb gb} editor)

    [:background {:color :white}
     [:padding {:all 10}
      [c/custom
       {:render
        (fn [el]
          (c/draw-text
            (debug-string ast)
            [0 0]
            :size 18
            :font :monospace)
          (comment c/draw-text
                   (debug-string cache)
                   [0 416]
                   :size 18
                   :font :monospace))}]]]))
