# (df "misc/frp4.janet")

(comment
  # (fset 'eval-df-thing (kmacro-lambda-form [S-right ?\( ?d ?f ?  ?\" ?m ?i ?s ?c ?/ ?f ?r ?p ?4 ?. ?j ?a ?n ?e ?t ?\" ?\) return S-left] 0 "%d"))
  # (global-set-key (kbd "C-c C-l") 'eval-df-thing)
  )


(use jaylib)

(import ./../src/extra_channel :as ec :fresh true)
(import ./../src/events :as e :fresh true)
(import ./../src/state :prefix "" :fresh true)
(import ./../src/keyboard :as kb :fresh true)

(def mouse     (ev/chan 100))
(def chars     (ev/chan 100))
(def keyboard  (ev/chan 100))
(def callbacks (ev/chan 100))
(def frame     (ev/chan 1))
(def rerender  (ev/chan 1))


(var delay-left @{})

(defn handle-keys
  [dt]
  (var k (get-char-pressed))
  
  (while (not= 0 k)
    (ec/push! chars @[:char k])
    (set k (get-char-pressed)))
  
  (loop [[k dl] :pairs delay-left
         :let [left ((update delay-left k - dt) k)]]
    (when (<= left 0)
      (ec/push! keyboard @[:key-down k])
      (put delay-left k repeat-delay)))
  
  (loop [k :in kb/possible-keys]
    (when (key-released? k)
      (put delay-left k nil))
    
    (when (key-pressed? k)
      (put delay-left k initial-delay)
      (ec/push! keyboard @[:key-down k]))))

(varfn handle-scroll
  []
  (let [move (get-mouse-wheel-move)]
    (when (not= move 0)
      (ec/push! mouse @[:scroll (* move 10)]))))

# table of callbacks, eg @{@[:down [10 10]  [|(print "hello") |(print "other")]}
#                          ^ a mouse event  ^ queued callbacks
#                                           ^ is actually a ev/chan
#                                           ^ but using struct to visualise
(def callbacks @{:changed false})

(defn push-callback!
  [ev cb]
  (-> callbacks
      (update ev (fn [chan]
                   (default chan (ev/chan 1))
                   (ec/push! chan cb)
                   chan))
      (put :changed true)))

(defn handle-callbacks
  [callbacks]
  (loop [[ev cbs] :pairs callbacks
         :when (not= ev :changed)]
    (e/pull-all cbs [apply]))
  
  (loop [k :in (keys callbacks)]
    (put callbacks k nil)))

(def button
  @{:rec [300 20 100 50]
    :color :green
    
    :cb (fn [self]
          (print "a " (self :color) " button was pressed"))
    
    :draw (fn draw-button
            [self]
            (draw-rectangle-rec (self :rec) (self :color)))
    
    :on-event (fn update-button
                [self ev]
                (def [kind data] ev)
                (case kind
                  :press (when (in-rec? data (self :rec))
                           (push-callback! ev |(:cb self)))))})

(def search-area @{})

(defn text-area-on-event
  [self ev]
  (match ev
    [:key-down k]
    (do
      (handle-keyboard2
        (self :gb)
        k))
    [:char k]
    (handle-keyboard-char
      (self :gb)
      k)
    [:scroll n]
    (handle-scroll-event
      (self :gb)
      data)
    [:press _]
    (handle-mouse-event
      (self :gb)
      ev
      (fn [kind f]
        (push-callback! kind f)
        (put (self :gb) :changed true)))))

(def text-area
  @{:id :main
    
    :gb (merge-into gb-data
                    @{:search (fn [props]
                                (put focus :focus search-area)
                                (put focus :updated true))})
    
    :draw (fn [self]
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

(merge-into
  search-area
  @{:id :search
    
    :gb search-data
    
    :draw (fn [self]
            (rl-pop-matrix)
            
            #(end-texture-mode)
            
            (gb-pre-render (self :gb))
            
            #(begin-texture-mode (rt-ref :data))
            
            (rl-push-matrix)
            
            (rl-load-identity)
            
            #(rl-scalef 2 2 1)
            
            (gb-render-text (self :gb)))
    
    :on-event (fn [self ev]
                (pp ev)
                (text-area-on-event self ev))})

(def caret
  @{:draw (fn [self]
            (when-let [gb (self :gb)]
              (render-cursor gb)))
    
    :on true
    
    :on-event (fn [self ev]
                (match ev
                  {:focus focus}
                  (put self :gb (focus :gb))
                  
                  [:dt dt]
                  (when (self :gb)
                    (when (and (> ((self :gb) :blink) 30)
                               (self :on))
                      (put self :on false))
                    
                    (when (> ((self :gb) :blink) 60)
                      (set ((self :gb) :blink) 0)
                      (put self :on true)))))})

(put focus :focus text-area)
(put focus :changed true)

(def button2
  (table/setproto
    @{:rec [350 10 100 50]
      :color :blue}
    button))

(varfn render
  [dt]
  (:draw text-area)
  (:draw search-area)
  (:draw caret)
  
  (:draw button)
  (:draw button2))

(def dependencies
  @{mouse @[button button2 text-area search-area]
    keyboard @[|(:on-event (focus :focus) $)]
    chars @[|(:on-event (focus :focus) $)]
    focus @[caret]
    callbacks @[pp handle-callbacks]})

(comment
  (get-in focus [:focus :id])
  
  (loop [[pullable pullers] :pairs dependencies]
    (when-let [hi-i (find-index |(and (table? $) ($ :history)) pullers)]
      (def history (pullers hi-i))
      (array/remove pullers hi-i)
      (case (type pullable)
        :core/channel (e/pull-all (history :history) [pullable])
        :table        (do (loop [k :in (keys pullable)]
                            (put pullable k nil))
                        (merge-into pullable (history :history)))
        (error (string "Strange" (type pullable))))
      (array/push pullers history)))
  #
  )

# (e/record-all dependencies)
# need to make gb-data not contain circular references etc

(def finally
  @{frame [render caret]})

(varfn draw-frame
  [dt]
  (handle-keys dt)
  (handle-scroll)
  
  (ec/push! frame @[:dt dt])
  
  (when (mouse-button-pressed? 0)
    # uses arrays in order to have reference identity rather than value identity
    # relevant for callback handling
    (ec/push! mouse @[:press (get-mouse-position)]))
  
  (e/pull-deps dependencies finally))

(comment
  (ec/push! mouse [:down [10 10]])
  )

