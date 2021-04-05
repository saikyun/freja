(use ./src/highlighting)
(use ./src/new_gap_buffer)

(defn styling-worker
  [parent]
  (print "New thread started!")
  (def content (thread/receive))
  (print "got message!")
  (def res (peg/match styling-grammar content))
  (:send parent [:hl res]))

(def thread (thread/new styling-worker 32))
(:send thread (content gb-data))

(comment
  (try
    (let [[kind res] (thread/receive 0)]
      (print kind)
      (case kind
        :hl
        (-> gb-data
            (put :highlighting res)
            (put :changed true))
        
        # else
        (print "unmatched message"))
      :ok)
    ([err fib]))
  
  (gb-data :highlighting)
  
  (print "more?"))
