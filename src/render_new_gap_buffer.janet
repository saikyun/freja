(import spork/test)
(use jaylib)
(import ./new_gap_buffer :prefix "")
(import ./textfield_api :prefix "")
(import ./text_rendering :prefix "")

(defn index-passing-max-width
  "Returns the index of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [sizes gb start stop max-width]
  (var ret nil)
  (var acc-w 0)
  
  (gb-iterate gb
              start stop
              i c
              (let [[w h] (sizes c)]
                (+= acc-w w)
                (when (dyn :debug)
                  (print "c: " c " - " "acc-w: " acc-w))
                (when (> acc-w max-width) # we went too far!
                  (set ret i)
                  (break))))
  
  ret)

(comment
  (with-dyns [:debug true]
    (let [sizes (text-data :sizes)
          gb {:text @"abc"
              :gap-start 0
              :gap-stop 0
              :gap @"123"}]
      (index-passing-max-width sizes gb
                               0 3
                               20)))
  )

(varfn word-wrap-gap-buffer
  [sizes lines gb start stop width h y y-limit]
  (array/clear lines)
  
  (var x 0)
  (var y y)
  (var w 0)
  
  (var s (buffer/new 1))
  (var beginning-of-word-i 0)
  
  (gb-iterate
    gb
    start stop
    i c
    (case c newline
      (do (array/push lines (inc i))
        (+= y h)
        (set x 0)
        (set beginning-of-word-i (inc i)))
      
      space
      (let [old-w w]
        (set w 0)
        
        (if (> (+ x old-w) width) ## we went outside the max width
          (do ## so rowbreak before the word
            (array/push lines beginning-of-word-i)
            (+= y h)
            (set x old-w)
            
            ## is the word also longer than the line?
            (when (> (+ x old-w) width)
              ## then we need to break up the word
              (var break-i beginning-of-word-i)
              (while (set break-i (index-passing-max-width
                                    sizes
                                    gb
                                    break-i
                                    i
                                    width))
                (array/push lines break-i))))
          
          (+= x (+ old-w (first (sizes c)))))
        
        (set beginning-of-word-i (inc i)))
      
      (let [new-w (+ w (first (sizes c)))]
        (set w new-w)))


    (when (> y y-limit)
      (return stop-gb-iterate)))
  
  (when (not (> y y-limit))
    (let [old-w w]
      (set w 0)
      
      (if (> (+ x old-w) width) ## we went outside the max width
        (do ## so rowbreak before the word
          (array/push lines beginning-of-word-i)
          (+= y h)
          (set x old-w)
          
          ## is the word also longer than the line?
          (when (> (+ x old-w) width)
            ## then we need to break up the word
            (var break-i beginning-of-word-i)
            (while (set break-i (index-passing-max-width
                                  sizes
                                  gb
                                  break-i
                                  stop
                                  width))
              (array/push lines break-i))))))
    
    (array/push lines stop))
  
  lines)

(var debug nil)
(set debug false)

(varfn render-lines
  [sizes conf gb lines start y h]
  
  (var y y)
  
  (when debug
    (print "rendering lines")
    (print "from " start " to ")
    (pp lines))
  
  (var last start)
  
  (var s (buffer/new 1))
  
  (var x 0)
  
  (loop [l :in lines]
    (do (set x 0) 
      (gb-iterate
        gb
        last l
        i c
        (let [[w h] (sizes c)]
          (put s 0 c)
          (when debug
            (print s)
            (print "x: " x " - y: " y))
          (draw-text conf s [x y] :black)
          (+= x w))))
    (+= y h)
    (when debug
      (print))
    (set last l))
  
  x)

(varfn gb-render-text
  [props]
  (def {:w w
        :sizes sizes
        :conf conf
        :colors colors
        :scroll scroll} props)
  
  (def gb props)
  
  (def lines @[])
  
  
  (def lines (let [sizes sizes
                   lines @[]]
               (word-wrap-gap-buffer
                 sizes
                 lines
                 gb
                 0 (gb-length gb)
                 1920
                 14
                 0
                 1080)))
  
  (render-lines sizes conf gb lines 0 0 14))

(comment
  (do (def gb {:text @"a b c "
               :gap-start 0
               :gap-stop 0
               :gap @"1 2 3 "})
    (def lines (let [sizes (text-data :sizes)
                     lines @[]]
                 (word-wrap-gap-buffer
                   sizes
                   lines
                   gb
                   0 (gb-length gb)
                   30
                   14
                   0
                   1000)))
    
    (var last-i 0)
    (loop [l :in lines]
      (gb-iterate gb
                  last-i l
                  i c
                  (prin (-> (buffer/new 1)
                            (buffer/push-byte c))))
      (print)
      (set last-i l)))
  
  
  
  )
