(import ../freja/new_gap_buffer :prefix "")
(import ../freja/new_gap_buffer_util :prefix "")
(import spork/test)

(comment
  ### Structure of a gap buffer
  (def wat @{:text @""    ## the last committed string
             :gap-start 0 ## position relative to `:text` that marks the beginning of the gap
             :gap-stop 0  ## position relative to `:text` that marks the end of the gap
             :caret 0     ## absolute position of caret
             :selection 0 ## absolute position of caret
             :gap @""     ## contents of the gap
             })
  
  ### Example
  
  ## string representation of the gap
  (def gb-s "ab|[c]")
  
  # | means :caret
  # [ means :gap-start
  # ] means :gap-end
  
  
  (deep= (string->gb gb-s)
         @{:actions @[]
           :caret 2
           :gap @"c"
           :gap-start 2
           :gap-stop 2
           :redo-queue @[]
           :text @"ab"})
  #=> true
  
  
  ## More details about string representation of gap buffer below
  
  
  ### ----- Caret
  
  ### | means "caret"
  (deep= (string->gb "ab|c")
         @{:actions @[]
           :caret 2
           :gap @""
           :gap-start 2
           :gap-stop 2
           :redo-queue @[]
           :text @"abc"})
  #=> true
  
  ### if there is no |, assume the caret is at the end of the string
  (-> (string->gb "abc")
      render)
  #=> "abc|"
  
  
  ### ----- Selection
  
  # * means "start of selection"
  # in this case, we select from right to left
  (deep= (string->gb "ab|c*")
         @{:actions @[]
           :caret 2
           :gap @""
           :gap-start 2
           :gap-stop 2
           :redo-queue @[]
           :selection 3
           :text @"abc"})
  #=> true
  
  #...and select from left to right
  (deep= (string->gb "ab*c|")
         @{:actions @[]
           :caret 3
           :gap @""
           :gap-start 3
           :gap-stop 3
           :redo-queue @[]
           :selection 2
           :text @"abc"})
  #=> true
  
  
  ### ----- Gap
  
  ### [xyz] means "xyz is currently in the gap
  (deep= (string->gb "a[xyz]b|")
         @{:actions @[]
           :caret 5
           :gap @"xyz"
           :gap-start 1
           :gap-stop 1
           :redo-queue @[]
           :text @"ab"})
  #=> true
  
  ### if there is no |, but there is a gap, assume the caret is at the end of the gap
  (deep= (string->gb "a[xyz]b")
         @{:actions @[]
           :caret 5
           :gap @"xyz"
           :gap-start 1
           :gap-stop 1
           :redo-queue @[]
           :text @"ab"})
  #=> true
  
  ### if there is no [] but there is a |, that means there's an empty gap at the position of the caret
  (string->gb     "ab|c")
  #=> (string->gb "ab[]|c")
  
  ### (ab) means that "ab has been deleted" (e.g. using backspace)
  (deep= (string->gb "(ab)|")
         @{:actions @[]
           :caret 0
           :gap @""
           :gap-start 0
           :gap-stop 2
           :redo-queue @[]
           :text @"ab"})
  #=> true
  
  # this is more apparent when running `commit!` and `render`
  (-> (string->gb "(ab)|")
      commit!
      render)
  #=> "|"
  
  ### (ab[123]cd) means ab and cd has been deleted, while 123 is in the buffer
  (deep= (string->gb "(ab[123]cd)")
         @{:actions @[]
           :caret 3
           :gap @"123"
           :gap-start 0
           :gap-stop 4
           :redo-queue @[]
           :text @"abcd"})
  #=> true

  
  (-> (string->gb "(ab[123]cd)")
      commit!
      render)
  #=> "123|"
  
  
  ### ----- More examples
  
  # we can combine the notation to create more complicated buffers
  
  (deep= (string->gb "ab*[ca|ca]")
         @{:actions @[]
           :caret 4
           :gap @"caca"
           :gap-start 2
           :gap-stop 2
           :redo-queue @[]
           :selection 2
           :text @"ab"})
  #=> true
  
  # running commit and render can make it easier to understand
  (-> (string->gb "ab*[ca|ca]")
      commit!
      render)
  #=> "ab*ca|ca"
  
  (deep= (string->gb "1(ignored[23|45]ignored)6*")
         @{:actions @[]
           :caret 3
           :gap @"2345"
           :gap-start 1
           :gap-stop 15
           :redo-queue @[]
           :selection 6
           :text @"1ignoredignored6"})
  #=> true
  
  
  ## spaces hould work
  (deep= (string->gb "a b|")
         @{:actions @[]
           :caret 3
           :gap @""
           :gap-start 3
           :gap-stop 3
           :redo-queue @[]
           :text @"a b"})
  #=> true
  )

