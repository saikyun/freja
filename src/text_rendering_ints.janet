(use jaylib)
(import ../build/text-rendering :prefix "")
(import ./text_rendering :prefix "")

(import spork/test)

(comment (def sizes @{84 [8.25 14] 44 [8.25 14] 87 [8.25 14] 189 [8.25 14]
                      59 [8.25 14] 137 [8.25 14]
                      164 [8.25 14] 99 [8.25 14] 100 [8.25 14] 70 [8.25 14]
                      63 [8.25 14] 118 [8.25 14] 163 [8.25 14]
                      32 [8.25 14] 141 [8.25 14] 177 [8.25 14] 186 [8.25 14]
                      66 [8.25 14] 132 [8.25 14] 113 [8.25 14]
                      187 [8.25 14] 148 [8.25 14] 191 [8.25 14] 108 [8.25 14]
                      138 [8.25 14] 52 [8.25 14] 101 [8.25 14]
                      47 [8.25 14] 50 [8.25 14] 93 [8.25 14] 142 [8.25 14]
                      169 [8.25 14] 56 [8.25 14] 81 [8.25 14]
                      121 [8.25 14] 140 [8.25 14] 175 [8.25 14] 122 [8.25 14]
                      68 [8.25 14] 152 [8.25 14] 128 [8.25 14]
                      166 [8.25 14] 116 [8.25 14] 34 [8.25 14] 131 [8.25 14]
                      54 [8.25 14] 133 [8.25 14] 42 [8.25 14]
                      76 [8.25 14] 35 [8.25 14] 74 [8.25 14] 174 [8.25 14]
                      180 [8.25 14] 61 [8.25 14] 123 [8.25 14]
                      89 [8.25 14] 78 [8.25 14] 39 [8.25 14] 98 [8.25 14] 126
                      [8.25 14] 65 [8.25 14] 55 [8.25 14] 
                      195 [8.25 14] 91 [8.25 14] 57 [8.25 14] 62 [8.25 14] 88 [8.25 14]
                      125 [8.25 14] 171 [8.25 14]
                      10 [0 14] 139 [8.25 14] 173 [8.25 14] 161 [8.25 14] 79 [8.25 14]
                      129 [8.25 14] 80 [8.25 14]
                      135 [8.25 14] 49 [8.25 14] 157 [8.25 14] 48 [8.25 14]
                      150 [8.25 14] 176 [8.25 14] 43 [8.25 14]
                      194 [8.25 14] 156 [8.25 14] 46 [8.25 14] 110 [8.25 14]
                      53 [8.25 14] 83 [8.25 14] 153 [8.25 14]
                      40 [8.25 14] 120 [8.25 14] 147 [8.25 14] 96 [8.25 14]
                      134 [8.25 14] 104 [8.25 14] 143 [8.25 14] 
                      106 [8.25 14] 103 [8.25 14] 51 [8.25 14] 181 [8.25 14]
                      172 [8.25 14] 97 [8.25 14] 155 [8.25 14] 
                      109 [8.25 14] 178 [8.25 14] 179 [8.25 14] 190 [8.25 14]
                      115 [8.25 14] 136 [8.25 14] 119 [8.25 14]
                      162 [8.25 14] 185 [8.25 14] 72 [8.25 14] 94 [8.25 14]
                      168 [8.25 14] 184 [8.25 14] 151 [8.25 14] 
                      170 [8.25 14] 154 [8.25 14] 117 [8.25 14] 41 [8.25 14]
                      188 [8.25 14] 64 [8.25 14] 67 [8.25 14]
                      73 [8.25 14] 124 [8.25 14] 95 [8.25 14] 75 [8.25 14]
                      159 [8.25 14] 111 [8.25 14] 45 [8.25 14]
                      112 [8.25 14] 85 [8.25 14] 114 [8.25 14] 158 [8.25 14]
                      167 [8.25 14] 165 [8.25 14] 146 [8.25 14]
                      107 [8.25 14] 149 [8.25 14] 77 [8.25 14] 71 [8.25 14]
                      160 [8.25 14] 182 [8.25 14] 86 [8.25 14]
                      105 [8.25 14] 33 [8.25 14] 102 [8.25 14] 144 [8.25 14]
                      36 [8.25 14] 90 [8.25 14] 69 [8.25 14] 
                      38 [8.25 14] 145 [8.25 14] 37 [8.25 14] 130 [8.25 14]
                      183 [8.25 14] 60 [8.25 14] 58 [8.25 14]
                      82 [8.25 14]}))

(defn chars->sizes
  [conf chars]
  (table ;(array/concat ;(map (fn [c size]
                                @[c size])
                              chars (measure-each-char conf chars)))))

(def org-str @``
  hello there mr arnold 
  how are you doing? 
  I hope you're doing okay 
  I know I'm not 
  You know... 
  My wife... 
  She.. 
  She... 
  She's inhaled snow. 
  Multiple times. 
  I just don't know what to do. 
  It feels like my life is over. 
  It will never end, this feeling of misery, the feeling of helplesness. The reason being that I'm simply not good enough for her. I probably never were. 
  And now snow is all she wants.``)

















(def newline (first "\n"))
(def space (first " "))
(def needs-wrapping -1)

(varfn backward-lines-until-limit-original
  [lines sizes width top-y x y chars]
  
  (array/clear lines)
  (array/push lines (length chars))
  
  (var should-be-wrapped false)
  
  (var x x)
  (var y y)
  
  (def h ((sizes (first "a")) 1))
  
  (loop [i :down-to [(dec (length chars)) 0]
         :let [c (chars i)]
         :while (> y top-y)]
    (if (= newline c)
      (do (-= y h)
        (when should-be-wrapped
          (array/push lines needs-wrapping)
          (set should-be-wrapped false))
        (set x width)
        (array/push lines i))
      
      (do (def new-x (- x (first (sizes c))))
        (if (neg? new-x)
          (do (-= y h)
            (set should-be-wrapped true)
            (set x width))
          (set x new-x)))))
  
  lines)

(defn index-passing-max-width
  "Returns the index of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [sizes chars start stop max-width]
  (var ret nil)
  (var acc-w 0)
  (loop [i :range [start stop]
         :let [c (chars i)
               [w h] (sizes c)]]
    (+= acc-w w)
    (when (> acc-w max-width) # we went too far!
      (set ret i)
      (break)))
  ret)

(comment
  (var i 0) 
  (while i
    (print (set i (index-passing-max-width sizes "01234567890" i 10 40))))
  
  (let [i ]
    (index-passing-max-width sizes "01234567890" i 10 40))
  )

(varfn word-wrap
  [sizes lines chars start stop width h y y-limit]
  (array/clear lines)
  
  (var x 0)
  (var y y)
  (var w 0)
  
  (var s (buffer/new 1))
  (var beginning-of-word-i 0)
  
  (loop [i :range [start stop]
         :let [c (chars i)]
         :while (< y y-limit)]
    (case c newline
      (do (array/push lines (inc i))
        (+= y h)
        (set x 0)
        (set beginning-of-word-i (inc i)))
      
      space
      (let [old-w w]
        (set w 0)
        
        ## TODO: need to handle "space end of line"
        (if (> (+ x old-w) width) ## we went outside the max width
          (do ## so rowbreak before the word
            (print "first word wrap")
            
            (array/push lines beginning-of-word-i)
            (+= y h)
            (set x old-w)
            
            ## is the word also longer than the line?
            (when (> (+ x old-w) width)
              ## then we need to break up the word
              (var break-i beginning-of-word-i)
              (while (set break-i (index-passing-max-width
                                    sizes
                                    chars
                                    break-i
                                    i
                                    width))
                (array/push lines break-i))))
          
          (+= x (+ old-w (first (sizes c)))))
        
        (set beginning-of-word-i (inc i)))
      
      (let [new-w (+ w (first (sizes c)))]
        (set w new-w))))
  
  lines)

(comment
  (do (def lines2 @[])
    (def str "hej ye\ncool my due wahat u uuu")
    (word-wrap lines2 str
               0 (length str)
               50
               14
               0
               500
               )
    (render-lines (text-data :conf) str lines2 0 0 14)
    
    (let [leftovers-start (or (last lines2) 0)
          leftovers-end (length str)]
      (buffer/slice str leftovers-start leftovers-end)
      ))
  
  )

(var debug nil)
(set debug false)

(varfn render-lines
  [sizes conf chars lines start y h]
  
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
      (loop [i :range [last l]
             :let [c (chars i)
                   [w h] (sizes c)]]
        (put s 0 c)
        (when debug
          (prin s))
        (draw-text conf s [x y] :black)
        (+= x w)))
    (+= y h)
    (when debug
      (print))
    (set last l))
  
  x)

(defn joined-length
  [head tail]
  (+ (length head)
     (length tail)))

(defn joined-empty?
  [head tail]
  (= (joined-length head tail) 0))

(varfn joined-nth
  ``
  Fetches the element at `i` when the `head` and reversed `tail` are joined together.
  ``
  [head tail i]
  (if (< i (length head))
    (head i)
    (tail (- (dec (length tail)) (- i (length head))))))

(defn index-passing-max-width-joined
  "Returns the index of the char exceeding the max width.
Returns `nil` if the max width is never exceeded."
  [head tail sizes start stop max-width]
  (var ret nil)
  (var acc-w 0)
  (loop [i :range [start stop]
         :let [c (joined-nth head tail i)
               [w h] (sizes c)]]
    (+= acc-w w)
    (when (> acc-w max-width) # we went too far!
      (set ret i)
      (break)))
  ret)

(varfn word-wrap-backwards
  [sizes
   lines
   
   leftover-chars
   leftover-start   
   leftover-stop
   
   chars
   start
   stop
   width
   h
   x
   y
   y-limit]
  
  (array/clear lines)
  
  (var x 0)
  (var y y)
  (var w 0)
  
  (var s (buffer/new 1))
  (var beginning-of-word-i 0)
  
  
  (def joined-start leftover-start)  
  (def joined-stop (- (joined-length leftover-chars chars)
                      (- (length chars) stop)))  
  
  (when debug
    (print "hue? " 
           (- (length leftover-chars) joined-start)
           " hhh " joined-start " - " joined-stop)  
    
    (print (buffer/slice (string leftover-chars
                                 (string/reverse chars))
                         joined-start
                         joined-stop)))  
  
  (loop [i :range [joined-start joined-stop] 
         :let [c (joined-nth leftover-chars chars i)]
         :while (< y y-limit)]
    (case c newline
      (do (array/push lines (inc i))
        (+= y h)
        (set x 0)
        (set beginning-of-word-i (inc i)))
      
      space
      (let [old-w w]
        (set w 0)
        
        ## TODO: need to handle "space end of line"
        (if (> (+ x old-w) width) ## we went outside the max width
          (do ## so rowbreak before the word
            (print "second word wrap")

            (array/push lines beginning-of-word-i)
            (+= y h)
            (set x old-w)
            
            ## is the word also longer than the line?
            (when (> (+ x old-w) width)
              ## then we need to break up the word
              (var break-i beginning-of-word-i)
              (while (set break-i (index-passing-max-width-joined
                                    leftover-chars
                                    chars
                                    sizes
                                    break-i
                                    i
                                    width))
                (array/push lines break-i))))
          
          (+= x (+ old-w (first (sizes c)))))
        
        (set beginning-of-word-i (inc i)))
      
      (let [new-w (+ w (first (sizes c)))]
        (set w new-w))))
  
  (if (and (not (joined-empty? leftover-chars chars))
           (not= space (joined-nth leftover-chars chars (dec joined-stop)))
           (> (+ w x) width)) ## we went outside the max width
    (do ## so rowbreak before the word
      (array/push lines beginning-of-word-i)
      (+= y h)
      
      ## is the word also longer than the line?
      (if (> (+ w x) width)
        ## then we need to break up the word
        (do (var break-i beginning-of-word-i)
          (while (set break-i (index-passing-max-width-joined
                                leftover-chars
                                chars
                                sizes
                                break-i
                                joined-stop
                                width))
            (array/push lines break-i)))
        
        (set x (+ x w)))))
  
  (array/push lines joined-stop)
  
  (comment
    (set w 0)
    (when (> x width) ## TODO: need to handle "space end of line"
      (array/push lines beginning-of-word-i)
      (+= y h)
      (set x 0))
    
    (when (< y y-limit)
      (array/push lines stop)
      ))
  
  lines)

(comment
  
  (def ls @[])
  
  (def s1 "ab c")
  (def s2 " 6 54 ")
  
  (do
    (def b @"")
    
    (loop [i :range [0 (joined-length s1 s2)]]
      (buffer/push b (joined-nth s1 s2 i)))
    
    b)
  
  
  
  (do (def s1 "ab c") 
    (def s2 " 6 54 ")
    
    (print (string s1 (string/reverse s2)))
    
    (word-wrap-backwards 
      ls
      
      s1
      0
      (length s1)
      
      s2
      0
      0 #(length s2)
      
      40                        #w
      14
      0
      0
      300)
    
    (render-lines-backwards 
      (text-data :conf)
      
      s2
      
      s1
      0
      (length s1)
      
      ls
      (dec (length s2))
      0 0 14)
    
    
    
    
    
    )
  
  (word-wrap-backwards 
    ls
    
    "jag gillar att"
    0
    13
    
    "klappa katter massor"
    0
    19
    
    20 #w
    14
    0
    0
    300)
  
  
  
  
  
  
  "hej" "gid åp"
  #012   012345
  
  (def last (+ start # 5
               (- leftover-stop leftover-start)))
  (def ls @[0])
  
  )

(varfn render-caret
  [props x y h]
  (when (< (props :blink) 30)
    (draw-line-ex
      [x y]
      [x (+ y h)]
      1
      (get-in props [:colors :caret]))))

(varfn refresh-caret
  [props]
  (+= (props :blink) 1.1)
  (when (> (props :blink) 60) (set (props :blink) 0)))

(varfn render-lines-backwards
  [props
   sizes
   conf
   
   chars
   
   leftover-chars
   leftover-start
   leftover-stop
   
   lines start x y h]
  
  (var y y)
  (var x x)
  
  (when false
    (print "rendering lines")
    (print "from " start " to ")
    (pp lines))
  
  
  (var in-leftovers (not (nil? leftover-start)))  
  (var leftover-diff (if leftover-start
                       (- leftover-stop leftover-start)
                       0))
  
  (var last-break leftover-start)
  
  (var s (buffer/new 1))
  
  (loop [l :in lines]
    (set x 0)
    
    (loop [i :range [last-break l]
           :let [c (joined-nth leftover-chars chars i)
                 [w h] (sizes c)]]
      (put s 0 c)
      (draw-text conf s [x y] :black)
      (when (= i leftover-stop)
        (render-caret props x y h))
      (+= x w))
    
    (+= y h)
    (set last-break l))
  
  (-= y h) ## revert the last movement down
  
  (when (= (last lines) leftover-stop)
    (render-caret props x y h))
  
  y)

(varfn render-text
  [props]
  (def {:w w
        :sizes sizes
        :text text
        :after after
        :conf conf
        :colors colors
        :scroll scroll} props)
  
  (def w 1920)
  #(def w 50)
  
  (def lines @[])
  
  (var s (buffer/new 1))
  
  (refresh-caret props)
  
  (do
    (def y-limit 1080)
    
    (print "lines")
    (test/timeit 
      (backward-lines-until-limit lines sizes w (- y-limit) w 0 text))
    
    (var end 0)
    (var end-i nil)
    
    #(pp lines)
    
    (comment
      (when (not (empty? lines))
        (while (neg? (lines end))
          (+= end 1))))
    
    (def h ((sizes (first "a")) 1))
    (var x 0)
    (var y 0 #(- (* 0.5 (* h (length lines))))
      )
    (var i (- (length lines) 2)) ### the last one is rendered using `last`
    (var last (last lines))
    
    
    
    (while (and (<= end i)
                (< y y-limit))
      (let [l (lines i)
            prev (when (< i (- (length lines) 1))
                   (lines (inc i)))]
        
        (if (= l needs-wrapping)
          (when (= i 0) ## last line
            (let [wrapped-lines @[]]
              (word-wrap sizes wrapped-lines text last (length text) w h y y-limit)
              (array/pop wrapped-lines)
              (set x (render-lines sizes conf text wrapped-lines last y h))
              (when (not (empty? wrapped-lines))
                (set end-i (wrapped-lines
                             (dec (length wrapped-lines)))))
              
              (+= y (* h (length wrapped-lines)))
              #(pp wrapped-lines)
              (-= i 1)))                              
          
          (do (cond (= prev needs-wrapping)
                (let [wrapped-lines @[]]
                  (word-wrap sizes wrapped-lines text last l w h y y-limit)
                  (set x (render-lines sizes conf text wrapped-lines last y h))
                  (+= y (* h (dec (length wrapped-lines))))
                  (-= i 1)) ## skip the needs-wrapping value
                
                # else
                (do (set x 0)
                  (loop [i :range [last l]
                         :let [c (text i)
                               [w h] (sizes c)]]
                    (put s 0 c)
                    (draw-text conf s [x y] :black)
                    (+= x w)))
                
                #(pp r)
                
                #(print (buffer/slice text last l))
                )
            
            (set last l)
            (set end-i l)
            (+= y h)))
        
        (-= i 1))
      
      #(print y)
      )
    
    #(-= y h) ## move back one line since otherwise we end up below the current line
    (set x 0) 
    (set end-i (or end-i 0))
    
    (print "second part")
    (when (< y y-limit)
      (let [wrapped-lines @[]]
        (word-wrap-backwards
          sizes
          wrapped-lines
          
          text
          (if (empty? lines)
            0
            end-i)
          (length text)
          
          after
          0
          (length after)
          w
          h
          x
          y
          (+ y y-limit))

        (print "rendering123")
        (test/timeit
          (render-lines-backwards
            props
            sizes
            conf
            after
            
            text
            end-i
            (length text)
            
            wrapped-lines
            (if (empty? after)
              nil
              (dec (length after)))
            x
            y
            h))
        (+= y (* h (length wrapped-lines)))
        #(pp wrapped-lines)
        (-= i 1)))
    
    #(print (buffer/slice text last (length text)))
    ))

(comment
  
  (do (def new-str
        @``
        hello
        there
        i wonder what has happened it was such a long time ago    i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago   i wonder what has happened it was such a long time ago
        ``)
    
    
    (def width 1920)
    (def lines @[])
    
    
    (array/clear lines)
    (backward-lines-until-limit lines sizes width -1080 width 0 new-str))
  
  (do
    (def width 1920)
    (def lines @[])
    
    
    (def new-str (buffer (string/repeat org-str 100)))
    
    (test/timeit (do
                   (print "heh")
                   (backward-lines-until-limit lines sizes width -1080 width 0 new-str)
                   
                   (print "2")
                   (test/timeit
                     (do (def h ((sizes (first "a")) 1))
                       (var y 0)
                       (var i (dec (length lines)))
                       (var last (last lines))
                       (while (and (<= 0 i)
                                   (< y 1080))
                         (let [l (lines i)
                               prev (when (< i (dec (length lines)))
                                      (lines (inc i)))]
                           (when (not= l needs-wrapping)
                             
                             (if (= prev needs-wrapping)
                               (let [wrapped-lines @[]]
                                 #(print "needs wrapping!")
                                 (word-wrap wrapped-lines new-str last l width)
                                 (render-lines new-str wrapped-lines last)
                                 (+= y (* h (length wrapped-lines)))
                                 #(pp wrapped-lines)
                                 (-= i 1)) ## skip the needs-wrapping value
                               #(print "from " last " - to " l " : " (buffer/slice new-str last l))         
                               (print (buffer/slice new-str last l))
                               )
                             
                             (set last l))
                           (-= i 1))
                         (+= y h)
                         
                         #(print y)
                         )
                       
                       (print (buffer/slice new-str last (length new-str)))
                       )))))

  (do
    (def width 1920)
    (def lines @[])
    
    
    (def new-str (buffer (string/repeat org-str 100)))
    
    (test/timeit (do
                   (print "heh")
                   (test/timeit
                     (backward-lines-until-limit lines sizes width -1080 width 0 new-str))
                   
                   (print "2")
                   (test/timeit
                     (do (def h ((sizes (first "a")) 1))
                       (var y 0)
                       (var i (dec (length lines)))
                       (var last (last lines))
                       (while (and (<= 0 i)
                                   (< y 1080))
                         (let [l (lines i)
                               prev (when (< i (dec (length lines)))
                                      (lines (inc i)))]
                           (when (not= l needs-wrapping)
                             
                             (if (= prev needs-wrapping)
                               (let [wrapped-lines @[]]
                                 #(print "needs wrapping!")
                                 (word-wrap wrapped-lines new-str last l width)
                                 (render-lines new-str wrapped-lines last)
                                 (+= y (* h (length wrapped-lines)))
                                 #(pp wrapped-lines)
                                 (-= i 1)) ## skip the needs-wrapping value
                               #(print "from " last " - to " l " : " (buffer/slice new-str last l))         
                               #(print (buffer/slice new-str last l))
                               )
                             
                             (set last l))
                           (-= i 1))
                         (+= y h)
                         
                         #(print y)
                         )
                       
                       #(print (buffer/slice new-str last (length new-str)))
                       )))))
  
  (do
    (def width 1920)
    (def lines @[])
    
    (def new-str (buffer (string/repeat org-str 100)))
    
    (test/timeit (do
                   (print "heh")
                   (test/timeit
                     (backward-lines-until-limit lines sizes width -1080 width 0 new-str))
                   
                   (print "2")
                   (test/timeit
                     (do (def h ((sizes (first "a")) 1))
                       (var y 0)
                       (var i (dec (length lines)))
                       (var last (last lines))
                       (while (and (<= 0 i)
                                   (< y 1080))
                         (let [l (lines i)
                               prev (when (< i (dec (length lines)))
                                      (lines (inc i)))]
                           (when (not= l needs-wrapping)
                             
                             (if (= prev needs-wrapping)
                               (let [wrapped-lines @[]]
                                 #(print "needs wrapping!")
                                 (word-wrap wrapped-lines new-str last l width)
                                 (render-lines new-str wrapped-lines last)
                                 (+= y (* h (length wrapped-lines)))
                                 #(pp wrapped-lines)
                                 (-= i 1)) ## skip the needs-wrapping value
                               #(print "from " last " - to " l " : " (buffer/slice new-str last l))         
                               #(print (buffer/slice new-str last l))
                               )
                             
                             (set last l))
                           (-= i 1))
                         (+= y h)
                         
                         #(print y)
                         )
                       
                       #(print (buffer/slice new-str last (length new-str)))
                       )))))
  
  )











(comment
  
  (let [_ (update text-data :text |(string/repeat $ 100))]
    :ok)
  
  (let [_ (update text-data :after |(string/repeat $ 100))]
    :ok)
  
  (let [_ (test/timeit
            (content text-data))]
    123)
  
  
  
  )
