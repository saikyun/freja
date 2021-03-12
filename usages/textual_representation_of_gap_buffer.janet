(import ../src/new_gap_buffer :prefix "")
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
         @{:gap-stop 2
           :caret 2
           :gap-start 2
           :text @"ab"
           :gap @"c"})
  #=> true
  
  
  ## More details about string representation of gap buffer below
  
  
  ### ----- Caret
  
  ### | means "caret"
  (string->gb "ab|c")
  #=> @{:gap-stop 2 :caret 2 :gap-start 2 :text @"abc" :gap @""}
  
  ### if there is no |, assume the caret is at the end of the string
  (string->gb "abc")
  #=>  @{:gap-stop 3 :caret 3 :gap-start 3 :text @"abc" :gap @""}
  
  
  ### ----- Selection
  
  # * means "start of selection"
  # in this case, we select from right to left
  (string->gb "ab|c*")
  #=> @{:gap-stop 2 :caret 2 :gap-start 2 :text @"abc" :selection 3 :gap @""}
  
  #...and select from left to right
  (string->gb "ab*c|")
  #=> @{:gap-stop 3 :caret 3 :gap-start 3 :text @"abc" :selection 2 :gap @""}
  
  
  ### ----- Gap
  
  ### [xyz] means "xyz is currently in the gap
  (string->gb "a[xyz]b|")
  #=> @{:gap-stop 1 :caret 5 :gap-start 1 :text @"ab" :gap @"xyz"}
  
  ### if there is no |, but there is a gap, assume the caret is at the end of the gap
  (string->gb "a[xyz]b")
  #=>  @{:gap-stop 1 :caret 5 :gap-start 1 :text @"ab" :gap @"xyz"}
  
  ### if there is no [] but there is a |, that means there's an empty gap at the position of the caret
  (string->gb     "ab|c")
  #=> (string->gb "ab[]|c")
  
  ### (ab) means that "ab has been deleted" (e.g. using backspace)
  (string->gb "(ab)|")
  #=> @{:gap-stop 2 :caret 0 :gap-start 0 :text @"ab" :gap @""}
  
  # this is more apparent when running `commit!`
  (commit! (string->gb "(ab)|"))
  #=> @{:gap-stop 0 :caret 0 :gap-start 0 :text @"" :gap @""}
  
  ### (ab[123]cd) means ab and cd has been deleted, while 123 is in the buffer
  (string->gb "(ab[123]cd)")
  #=> @{:gap-stop 4 :caret 3 :gap-start 0 :text @"abcd" :gap @"123"}
  
  (commit! (string->gb "(ab[123]cd)"))
  #=> @{:gap-stop 3 :caret 3 :gap-start 3 :text @"123" :gap @""}
  
  
  ### ----- More examples
  
  # we can combine the notation to create more complicated buffers
  (string->gb "ab*[ca|ca]")
  #=> @{:gap-stop 2 :caret 4 :gap-start 2 :text @"ab" :selection 2 :gap @"caca"}
  
  # running commit can make it easier to understand
  (commit! (string->gb "ab*[ca|ca]"))
  #=> @{:gap-stop 6 :caret 4 :gap-start 6 :text @"abcaca" :selection 2 :gap @""}
  
  (string->gb "1(ab[23]cd)|")
  
  (string->gb "1(ignored[23|45]ignored)6*")
  #=> @{:gap-stop 15 :caret 3 :gap-start 1 :text @"1ignoredignored6" :selection 6 :gap @"2345"}
  

  )

