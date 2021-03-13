(import spork/test)
(use jaylib)
(import ./new_gap_buffer :prefix "")
(import ./textfield_api :prefix "")
(import ./text_rendering :prefix "")
(import ./find_row_etc :prefix "")

(varfn index-passing-max-width
  "Returns the index of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [gb start stop max-width]
  
  (def {:sizes sizes} gb)
  
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
  [gb start stop max-width]
  
  (var acc-w 0)
  (var last-w 0)
  
  (def {:sizes sizes} gb)
  
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
    (let [gb {:text @"abc"
              :sizes (gb-data :sizes) 
              :gap-start 0
              :gap-stop 0
              :gap @"123"}]
      (index-passing-max-width gb
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
  
  (array/clear lines)
  (array/clear y-poses)
  
  (gb-iterate
    gb
    start stop
    i c
    
    (case c newline
      (do (array/push lines i)
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
  [{:selection selection :caret caret} i]
  (when selection
    (let [start (min selection caret)
          stop (max selection caret)]
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
    (let [[w h] (sizes c)] (+= acc-w w)))
  
  acc-w)

(comment
  (width-between gb-data 0 10)
  
  (gb-iterate
    gb-data
    0 100
    i c
    (print i)
    )
  )

(varfn render-selection-box
  [gb start stop y]
  (def {:selection selection
        :caret caret
        :sizes sizes
        :offset offset
        :screen-scale screen-scale} gb)
  (def [x-scale y-scale] screen-scale)
  (when selection
    (let [sel-start (min selection caret)
          sel-stop  (max selection caret)]
      
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
            [(+ (offset 0) start-x) y (- stop-x start-x) (* y-scale line-h)]
            :white))))))

(varfn render-lines
  [sizes conf gb lines start-index start-y h y-limit]
  (def {:screen-scale screen-scale} gb)  
  (def [x-scale y-scale] screen-scale)
  
  (def start-y (* y-scale start-y))
  
  (var y start-y)
  
  (when debug
    (print "rendering lines")
    (print "from " start-index " to ")
    (pp lines))
  
  (var last start-index)
  
  (var s (buffer/new 1))
  
  (var x 0)
  
  (var i 0)
  
  (loop [l :in lines]
    (when (> y (- start-y h))
      (do (set x 0) 
        
        (++ i)
        
        ## TODO: If line in selection, draw selection box here
        (render-selection-box gb last l y)
        
        (def s2 (string/format "%d" i))
        
        (draw-text*2 conf s2 [0 y] :gray x-scale)
        
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
            (draw-text*2 conf s [(+ 30 x)
                                 y] (if (in-selection? gb i)
                                      :blue
                                      :black)
                         x-scale)
            (+= x (* x-scale w))))))
    (+= y h)
    
    (when debug
      (print))
    
    (set last l)))

(varfn current-line
  [gb]
  (def {:lines lines} gb)
  (def i (gb :caret))
  (let [line-index (max 0 (binary-search-closest
                            lines
                            |(compare i $)))]
    
    line-index))

(comment
  (current-line gb-data)
  )

(varfn index-above-cursor
  [gb]
  (let [{:lines lines
         :caret-pos caret-pos
         :memory-of-caret-x-pos mocxp} gb
        prev-line (dec (current-line gb))]
    (if (<= 0 prev-line)
      (or (index-passing-max-width
            gb
            (get lines (dec prev-line) 0) ## start of prev line
            (lines prev-line)             ## end of prev line
            mocxp)                        ## current x position
          (lines prev-line))
      0)))

(varfn index-below-cursor
  [gb]
  (let [{:lines lines
         :caret-pos caret-pos
         :memory-of-caret-x-pos mocxp} gb
        next-line (inc (current-line gb))]
    (if (< next-line (length lines))
      (or (index-passing-max-width
            gb
            (get lines (dec next-line) 0) ## start of next line
            (lines next-line)             ## end of next line
            mocxp)                        ## current x position
          (lines next-line))
      (gb-length gb))))

(varfn move-up!
  [gb]
  (print "new pos: " (index-above-cursor gb))
  (put-caret gb (index-above-cursor gb)))

(varfn move-down!
  [gb]
  (put-caret gb (index-below-cursor gb)))

(comment
  (index-above-cursor gb-data)
  (index-below-cursor gb-data)
  (move-up! gb-data)
  (move-down! gb-data)
  )

(varfn index->pos
  [gb index]
  (def {:lines lines
        :y-poses y-poses
        :conf conf} gb)
  (let [line-index (-> (max 0 (binary-search-closest
                                lines
                                |(compare index $)))
                       (min (max 0 (dec (length lines)))))
        x (width-between
            gb
            (get lines (dec line-index) 0)
            index)
        y (y-poses line-index)]
    
    (when (dyn :debug)
      (print "line-index: " line-index))
    
    [x y]))

(comment
  (gb-data :conf)
  (gb-data :y-poses)
  
  (with-dyns [:debug true]
    (index->pos gb-data (gb-data :caret)))
  )

(varfn gb-pre-render
  [gb]
  (def {:position position
        :offset offset
        :sizes sizes
        :size size
        :conf conf
        :colors colors
        :scroll scroll
        :changed changed
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :lines lines
        :y-poses y-poses
        :scroll scroll}
    gb)
  
  (def [x-scale _ _ _ _ y-scale] (get-screen-scale))  
  
  (def [x y] position)
  (def [w h] size)
  
  (def screen-w (* x-scale (get-screen-width)))
  (def screen-h (* y-scale (get-screen-height)))
  
  (when (not (gb :texture))
    (put gb :texture (load-render-texture (* x-scale w)
                                          (* y-scale h) #screen-w screen-h
                                          )))
  
  (when (or changed changed-nav changed-selection)
    (test/timeit
      (do
        (def lines (or lines @[]))
        (put gb :lines lines)      
        
        (def y-poses (or y-poses @[]))
        (put gb :y-poses y-poses)      
        
        (def lines (if changed
                     (let [sizes sizes]
                       (word-wrap-gap-buffer
                         gb
                         sizes
                         lines
                         y-poses
                         gb
                         0 (gb-length gb)
                         (size 0)
                         14
                         y
                         (- (size 1) (* y-scale scroll))))
                     lines))
        
        (when (or changed changed-selection)
          (begin-texture-mode (gb :texture))
          
          (rl-push-matrix)
          (clear-background (or (gb :background)
                                (colors :background)
                                #:blue
                                )
                            #(colors :background)
                            )
          
          (render-lines sizes conf gb lines 0 (+ y scroll) (* y-scale 14) (size 1))
          (rl-pop-matrix)
          
          (end-texture-mode))
        
        (put gb :caret-pos (index->pos gb (gb :caret)))
        
        (when changed-x-pos
          (put gb :memory-of-caret-x-pos (get-in gb [:caret-pos 0])))
        
        #        (pp (ez-gb gb))
        
        (put gb :changed false)
        (put gb :changed-x-pos false)
        (put gb :changed-nav false)
        (put gb :changed-selection false)))
    
    
    #(:texture (gb-data :texture))
    )             # Reset internal modelview matrix  
  )

(varfn gb-render-text
  [gb]
  (def {:position position
        :offset offset
        :sizes sizes
        :size size
        :conf conf
        :colors colors
        :scroll scroll
        :changed changed
        :changed-x-pos changed-x-pos
        :changed-nav changed-nav
        :changed-selection changed-selection
        :lines lines
        :y-poses y-poses
        :scroll scroll}
    gb)
  
  
  (def [x-scale _ _ _ _ y-scale] (get-screen-scale))  
  
  (def [x y] position)
  (def [w h] size)
  
  (def screen-w (* x-scale (get-screen-width)))
  (def screen-h (* y-scale (get-screen-height)))
  
  (rl-push-matrix)
  (rl-mult-matrixf-screen-scale)
  
  (draw-texture-pro
    (get-render-texture (gb :texture))
    [0 0 (* x-scale w)
     (* y-scale (- h)) # (- h) #screen-w (- screen-h)
     ]
    [0 0 w h  #(/ screen-w x-scale) (/ screen-h y-scale)
     ]
    [0 0]
    0
    :white)
  
  (+= (gb :blink) 1.1)
  (when (> (gb :blink) 60) (set (gb :blink) 0))
  
  (when-let [[x y] (and (< (gb :blink) 30)
                        (gb :caret-pos))]
    (draw-line-ex
      [(+ (* (conf :mult) (offset 0)) x) (+ y scroll)]
      [(+ (* (conf :mult) (offset 0)) x) (+ y 14 scroll)]
      1
      (get-in gb [:colors :caret])))
  
  (rl-pop-matrix))

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
