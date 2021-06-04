(import ./../misc/defonce :prefix "")
(import ./new_gap_buffer :prefix "")

(def state-ref @{:ch (ev/chan 1) :data @[] :only-last true})
(defonce screen-size-ref @{:ch (ev/chan 1) :data nil})
(defonce rt-ref @{:ch (ev/chan 1) :data nil})
(def render-queue-ref @{:ch (ev/chan 1) :data nil :only-last true})
(def mouse-pos-ref @{:ch (ev/chan 1) :data [-1 -1] :only-last true})
(def clicks-ref @{:ch (ev/chan 10) :no-history true})
(def scroll-ref @{:ch (ev/chan 10) :no-history true})
(def kb-ref @{:ch (ev/chan 10) :no-history true})
(def char-ref @{:ch (ev/chan 10) :no-history true})

(def new-frame-ref @{:ch (ev/chan 1) :no-history true :only-last true})
(def callbacks-ref @{:ch (ev/chan 10) :data @{}})
(defonce gb-ref @{:ch (ev/chan 1) :data nil :only-last true})
(defonce search-ref @{:ch (ev/chan 1) :data nil :only-last true})
(defonce file-open-ref @{:ch (ev/chan 1) :data nil :only-last true})
(defonce focus-ref @{:ch (ev/chan 1) :data nil :only-last true})


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
                  :position [5 24]
                  :offset [10 6]
                  :show-line-numbers true

                  :id :main})
  :ok)

(set file-open-data (new-gap-buffer))

(def comp-cols {:background 0x882491ff
                :text/color 0xffffffee
                :caret/color 0xffffff80})

(do (merge-into file-open-data
                @{:size [:max 24]
                  :position [0 24]
                  :offset [88 6]}
                comp-cols)
  :ok)

(set search-data (new-gap-buffer))

(do (merge-into search-data
                @{:size [:max 24]
                  :position [0 24]
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
