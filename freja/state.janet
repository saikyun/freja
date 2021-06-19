(import ./new_gap_buffer :prefix "")

(def freja-dir @"")

(def focus123 @{})


(defn ev/check
  [chan]
  (when (pos? (ev/count chan))
    (ev/take chan)))

(defn ev/push
  [chan v]
  (when (ev/full chan)
    (ev/take chan)) ## throw away old values
  (ev/give chan v))

(defn swap!
  [ref f & vs]
  (let [new-data (f (ref :data) ;vs)]
    (ev/push (ref :ch) new-data)
    (unless (ref :no-history)
      (put ref :data new-data))))


(var file-open-data nil)
(var search-data nil)

(var gb-data (new-gap-buffer))

(do (merge-into gb-data
                @{:size [800 :max]
                  :position [5 30]
                  :offset [10 6]
                  :show-line-numbers true

                  :id :main})
  :ok)

(set file-open-data (new-gap-buffer))

(def comp-cols {:background 0x882491ff
                :text/color 0xffffffee
                :caret/color 0xffffff80})

(do (merge-into file-open-data
                @{:size [:max 30]
                  :position [0 30]
                  :offset [88 6]}
                comp-cols)
  :ok)

(set search-data (new-gap-buffer))

(do (merge-into search-data
                @{:size [:max 30]
                  :position [0 28]
                  :offset [88 6]}
                comp-cols)
  :ok)


(def focus-checks @[])

(def updates @[])

(def draws @[])

(def fs @[])

(defn remove-f
  [f]
  (-?>> (find-index |(= $ f) fs)
        (array/remove fs)))

(defn add-f
  [f]
  (array/push fs f))
