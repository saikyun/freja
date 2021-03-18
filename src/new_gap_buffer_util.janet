(comment "Various functions not strictly needed, but useful while debugging.")

(varfn ez-gb
  [{:text text
    :gap-start gap-start
    :gap-stop gap-stop
    :gap gap
    :caret caret
    :selection selection
    :changed changed
    :changed-nav changed-nav
    :changed-selection changed-selection}]
  @{:text text
    :gap-start gap-start
    :gap-stop gap-stop
    :gap gap
    :caret caret
    :selection selection
    :changed changed
    :changed-nav changed-nav
    :changed-selection changed-selection})

(defn string->gb
  [s]
  (def gb-text
    ~{:caret     (replace (* ($) "|") ,(fn [pos] [:caret pos]))
      :selection (replace (* ($) "*") ,(fn [pos] [:selection pos]))
      :gap-content (* (/ ($) ,(fn [start] [:content-start start]))
                      "[" 
                      (any (+ :markers
                              (/ (<- :string) ,(fn [v] [:gap v]))))
                      "]"
                      (/ ($) ,(fn [stop] [:content-stop stop])))
      
      :gap-eat (* (/ ($) ,(fn [start]
                            [:gap-start start]))
                  "("
                  (any (+ :markers
                          (/ (<- :string) ,(fn [v] [:gap-eat v]))))
                  ")"
                  (/ ($) ,(fn [stop] [:gap-stop stop])))
      
      :string :w*
      :markers (+ :caret :selection :gap-content)
      :main (any (+ :gap-eat :markers (<- :string)))})
  
  (var i-to-remove 0)
  (def gap-eat @"")
  (def gb @{:gap @""
            :text @""
            :actions @[]
            :redo-queue @[]})
  (loop [v :in (peg/match gb-text s)]
    (if (string? v)
      (update gb :text buffer/push-string v)
      (let [[kind i] v]
        (case kind
          :gap (update gb :gap buffer/push-string i)
          
          :gap-eat (do (buffer/push-string gap-eat i)
                     (+= i-to-remove (length i))
                     (update gb :text buffer/push-string i))
          
          :gap-stop (do (put gb :gap-stop (dec (+
                                                 (length gap-eat)
                                                 (- (length (gb :gap)))
                                                 (- i
                                                    i-to-remove))))
                      (++ i-to-remove))
          
          :content-start
          (do (when (not (gb :gap-start))
                (put gb :gap-start (- i i-to-remove))
                (put gb :gap-stop (- i i-to-remove)))
            (++ i-to-remove))
          
          :content-stop
          (++ i-to-remove)
          
          (do
            (put gb kind (- i i-to-remove))
            (++ i-to-remove))))))  
  
  (when (not (gb :caret))
    (put gb :caret (- (+ (length (gb :text))
                         (length (gb :gap)))
                      (- (get gb :gap-stop 0)
                         (get gb :gap-start 0)))))
  
  (when (not (gb :gap-start))
    (put gb :gap-start (gb :caret))
    (put gb :gap-stop  (gb :caret)))
  
  (update gb :gap buffer)
  
  gb)


