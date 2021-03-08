(import spork/test)
(use jaylib)
(import ./new_gap_buffer :prefix "")
(import ./textfield_api :prefix "")
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")

(varfn index-passing-max-width
  "Returns the index of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [sizes gb start stop max-width]
  
  (var acc-w 0)
  
  (gb-iterate
    gb
    start stop
    i c
    (let [[w h] (sizes c)]
      (+= acc-w w)
      (when (dyn :debug)
        (print "i: " i " - c: " c " - " "acc-w: " acc-w))
      (when (> acc-w max-width) # we went too far!
        (return stop-gb-iterate i)))))

(varfn index-passing-middle-max-width
  "Returns the index of the middle of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [sizes gb start stop max-width]
  
  (var acc-w 0)
  (var last-w 0)
  
  (or (gb-iterate
        gb
        start stop
        i c
        (let [[w h] (sizes c)]
          (+= acc-w (+ (* w 0.5) last-w))
          (set last-w (* w 0.5))
          (when (dyn :debug)
            (print "i: " i " - c: " c " - " "acc-w: " acc-w))
          (when (> acc-w max-width) # we went too far!
            (return stop-gb-iterate i))))
      stop))

(comment
  (with-dyns [:debug true]
    (let [sizes (gb-data :sizes)
          gb {:text @"abc"
              :gap-start 0
              :gap-stop 0
              :gap @"123"}]
      (index-passing-max-width sizes gb
                               0 3
                               20)))
  )

(varfn word-wrap-gap-buffer
  [props
   
   sizes
   lines
   y-poses
   gb
   start
   stop
   width
   h
   y
   y-limit]
  
  (var x 0)
  (var y y)
  (var w 0)
  
  (var s (buffer/new 1))
  (var beginning-of-word-i 0)
  
  (def {:screen-scale screen-scale} gb)
  (def [x-scale _] screen-scale)
  
  (gb-iterate
    gb
    start stop
    i c
    
    (case c newline
      (do (array/push lines (inc i))
        (array/push y-poses y)
        (+= y h)
        (set x 0)
        (set w 0)
        (set beginning-of-word-i (inc i)))
      
      space
      (let [old-w w]
        (set w 0)
        
        (if (> (+ x old-w) width) ## we went outside the max width
          (do               ## so rowbreak before the word
            (when (not= beginning-of-word-i 0)
              (array/push lines beginning-of-word-i)
              (array/push y-poses y)
              (+= y h))
            (set x old-w)
            
            (when (> (+ x old-w) width)
              ## then we need to break up the word
              (var break-i beginning-of-word-i)
              (while (set break-i (index-passing-max-width
                                    sizes
                                    gb
                                    break-i
                                    i
                                    width))
                (array/push lines break-i)
                (array/push y-poses y)
                (+= y h))))
          
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
        (do                     ## so rowbreak before the word
          (when (not= beginning-of-word-i 0)
            (array/push lines beginning-of-word-i)
            (array/push y-poses y)
            (+= y h))
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
              (array/push lines break-i)
              (array/push y-poses y)
              (+= y h))))))
    
    (array/push lines stop)
    (array/push y-poses y))
  
  lines)

(var debug nil)
(set debug true)
(set debug false)

(varfn in-selection?
  [{:selection selection :gap-start gap-start} i]
  (when selection
    (let [start (min selection gap-start)
          stop (max selection gap-start)]
      (and (>= i start)
           (< i stop)))))

(varfn width-between
  [gb start stop]
  (def {:sizes sizes :screen-scale screen-scale} gb)
  (def [x-scale y-scale] screen-scale)
  (var acc-w 0)        
  
  (gb-iterate
    gb 
    start stop
    i c
    (let [[w h] (sizes c)] (+= acc-w (* x-scale w))))
  
  acc-w)

(comment
  (width-between gb-data 0 10)
  )

(varfn render-selection-box
  [gb start stop y]
  (def {:selection selection
        :gap-start gap-start
        :sizes sizes
        :screen-scale screen-scale} gb)
  (def [x-scale y-scale] screen-scale)
  (when selection
    (let [sel-start (min selection gap-start)
          sel-stop  (max selection gap-start)]
      
      (when (and (< sel-start stop)
                 (>= sel-stop start))
        
        (var line-h 0)
        (var start-x nil)
        (var stop-x nil)
        (var acc-w 0)        
        
        (gb-iterate
          gb
          start stop
          i c
          (let [[w h] (sizes c)]
            (when (and (nil? start-x)
                       (>= i sel-start))
              (set start-x acc-w))
            
            (+= acc-w (* x-scale w))
            
            (when (and start-x
                       (< i sel-stop))
              (set stop-x acc-w))
            
            (set line-h (max line-h h))))
        
        (when (and start-x
                   stop-x)
          
          (draw-rectangle-rec 
            [start-x y (- stop-x start-x) (* y-scale line-h)]
            :white))))))

(varfn render-lines
  [sizes conf gb lines start y h y-limit]
  (var y y)
  
  (when debug
    (print "rendering lines")
    (print "from " start " to ")
    (pp lines))
  
  (var last start)
  
  (var s (buffer/new 1))
  
  (var x 0)
  
  (def {:screen-scale screen-scale} gb)
  (def [x-scale _] screen-scale) 
  
  (loop [l :in lines]
    (when (> y (- h))
      (do (set x 0) 
        
        ## TODO: If line in selection, draw selection box here
        (render-selection-box gb last l y)
        
        (gb-iterate
          gb
          last l
          i c
          (let [[w h] (sizes c)]
            (put s 0 c)
            (when debug
              (print "last: " last " - l: " l)
              (prin s)
              (print (length s))
              #(print "x: " x " - y: " y)
              )
            (draw-text*2 conf s [x y] (if (in-selection? gb i)
                                        :blue
                                        :black)
                         x-scale)
            (+= x (* x-scale w))))))
    (+= y h)
    
    (when debug
      (print))
    
    (set last l)))

(varfn index->pos
  [gb index]
  (def {:lines lines
        :y-poses y-poses
        :conf conf} gb)
  (let [line-index (max 0 (binary-search-closest
                            lines
                            |(compare index $)))
        x (width-between 
            gb
            (get lines (dec line-index) 0)
            index)
        y (y-poses line-index)]
    
    (when (dyn :debug)
      (print "line-index:" line-index))
    
    [x y]))

(comment
  (gb-data :conf)
  (gb-data :y-poses)
  
  (with-dyns [:debug true]
    (index->pos gb-data (caret-pos gb-data)))
  )

(varfn gb-render-text
  [props]
  (def {:w w
        :sizes sizes
        :conf conf
        :colors colors
        :scroll scroll
        :changed changed
        :changed-nav changed-nav
        :changed-selection changed-selection
        :lines lines
        :y-poses y-poses
        :scroll scroll}
    props)
  
  (def gb props)
  
  (def [x-scale _ _ _ _ y-scale] (get-screen-scale))  
  
  (def w (* x-scale (get-screen-width)))
  (def h (* y-scale (get-screen-height)))
  
  (when (not (props :texture))
    (put props :texture (load-render-texture w h)))
  
  (when (or changed changed-nav changed-selection)
    (test/timeit
      (do
        (def lines (or lines @[]))
        (put props :lines lines)      
        (array/clear lines)
        
        (def y-poses (or y-poses @[]))
        (put props :y-poses y-poses)      
        (array/clear y-poses)
        
        (def lines (let [sizes sizes]
                     (word-wrap-gap-buffer
                       props
                       sizes
                       lines
                       y-poses
                       gb
                       0 (gb-length gb)
                       100
                       14
                       0
                       (- 2160 (* y-scale scroll)))))
        
        (when (or changed changed-selection)
          (begin-texture-mode (props :texture))
          
          (clear-background    #:blue
                               (colors :background)
                               )
          
          (render-lines sizes conf gb lines 0 (* y-scale scroll) (* y-scale 14) 1080)
          
          (end-texture-mode))
        
        (put props :caret-pos (index->pos props (caret-pos props)))
        
        (put props :changed false)
        (put props :changed-nav false)
        (put props :changed-selection false)))
    
    
    #(:texture (gb-data :texture))
    )             # Reset internal modelview matrix  
  
  (rl-load-identity)             # Reset internal modelview matrix  
  (rl-mult-matrixf-screen-scale)              
  
  (draw-texture-pro
    (get-render-texture (props :texture))
    [0 0 w (- h)]
    [0 0 (/ w x-scale) (/ h y-scale)]
    [0 0]
    0
    :white)
  
  (+= (props :blink) 1.1)
  (when (> (props :blink) 60) (set (props :blink) 0))
  
  (when-let [[x y] (and (< (props :blink) 30)
                        (props :caret-pos))]
    (draw-line-ex
      [(* x-scale x) (+ y scroll)]
      [(* x-scale x) (+ y 14 scroll)]
      1
      (get-in props [:colors :caret])))
  
  #  DrawTextureRec(target.texture, (Rectangle) { 0, 0, target.texture.width, -target.texture.height }, (Vector2) { 0, 0 }, WHITE);
  )

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
