(import freja-jaylib)
(import freja/state)
(import freja/textarea :as ta)
(use freja/defonce)
(import freja/theme :as t)
(import freja/new_gap_buffer :as gb)
(import freja/input)
(import freja/event/subscribe :as s)

(defmacro curry
  [f-sym curried-arg]
  ~(let [v ,curried-arg]
     (fn ,(symbol (string "curried-" f-sym))
       [& args]
       (,f-sym v ;args))))

(defonce state (ta/default-textarea-state))

(defonce state-big (ta/default-textarea-state))

(def text-size 20)

(defn bottom
  [props & _]
  [:background {:color 0x444444ff}
   [:padding {:all 2}
    [ta/textarea {#:height (min 120 (get props :bottom-size 200)) # 55
                  :min-height (min (+ 12 (* text-size 5))
                                   (+ 6 (get props :bottom-size 55)))
                  #:height 300
                  :state state
                  :text/spacing 0.5
                  :text/size text-size
                  :text/font "MplusCode"
                  :text/color (t/colors :text)}]]])

(defn big
  [_ & _]
  [:background {:color 0x444444ff}
   [:padding {:all 2}
    [ta/textarea {:state state-big
                  :text/spacing 0.5
                  :text/size text-size
                  :text/font "MplusCode"
                  :text/color (t/colors :text)}]]])

(varfn append
  [state s]
  (-> (state :gb)
      (gb/append-string! "\n")
      (gb/append-string! s)
      (gb/end-of-buffer)))

(varfn replace
  [state s]
  (def s (string/trim s))
  (def nof-rows (inc (length (string/find-all "\n" s))))

  (comment

    (def gb (state :gb))

    (update gb :printing-delay
            (fn [fib]
              (when fib
                (try
                  (when (fiber/can-resume? fib)
                    (cancel fib :print/canceled))
                  ([err fib]
                    (xprint stdout "canceled fib"))))

              (ev/spawn
                (do
                  (gb/replace-content gb "")

                  (gb/beginning-of-buffer gb)
                  (put gb :scroll 0)

                  (var wait 0)
                  (loop [c :in s]
                    (gb/append-char* gb c)
                    (put gb :changed true)
                    (+= wait (/ 0.002 nof-rows))
                    (when (> wait 0.01)
                      (try (ev/sleep wait)
                        ([err fib]
                          (unless (= err :print/canceled)
                            (propagate err fib))))
                      (set wait 0))))))))
  (-> (state :gb)
      (gb/replace-content s)
      (gb/beginning-of-buffer)
      (put :scroll 0)))

(varfn handle-eval-results
  [res]
  (when-let [code (res :code)]
    (print "=> " (string/trim code)))

  (if (res :error)
    (if-let [fib (res :fiber)]
      (debug/stacktrace fib
                        (or (res :msg) (res :error))
                        "")
      (do (print "no fiber")
        (eprintf "%p" (res :error))))
    (printf "%p" (res :value))))

(defn init
  []
  (s/put! state/editor-state
          :bottom bottom)

  (put state/editor-state
       :toggle-console
       (fn [self]
         (def curr (self :right))
         (if (= curr big)
           (s/put! self :right (self :last-right))
           (-> self
               (s/put! :last-right curr)
               (s/put! :right big)))))

  (s/subscribe! state/eval-results (fn [res] (handle-eval-results res)))
  (s/subscribe! state/out-events (curry replace state))
  (s/subscribe! state/out-events (curry append state-big)))

(defn toggle-console
  [_]
  (:toggle-console state/editor-state))

(defn clear-console
  [_]
  (gb/replace-content (state-big :gb) @""))
