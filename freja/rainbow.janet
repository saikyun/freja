(use ./new_gap_buffer)

(defn rgba->f
  [r g b & [a]]
  [(/ r 255)
   (/ g 255)
   (/ b 255)
   (or a 1)])

# M-x list-colors-display in emacs
(def colors
  @{1 0xff8c00ff # dark orange
    2 0xff1493ff # deep pink
    3 0x7fff00ff # chartreuse
    4 0x00bfffff # deep sky blue
    5 0xffff00ff # yellow
    6 0xda70d6ff # orchid
    7 0x00ff7fff # spring green
    8 0xff8247ff # siennal
    # repeat
    9 0xff8c00ff # dark orange
   10 0xff1493ff # deep pink
   11 0x7fff00ff # chartreuse
   12 0x00bfffff # deep sky blue
   13 0xffff00ff # yellow
   14 0xda70d6ff # orchid
   15 0x00ff7fff # spring green
   16 0xff8247ff # siennal
  })

(def colors
  @{1 0xff8c00ff # dark orange
    2 0xff1493ff # deep pink
    3 0x7fff00ff # chartreuse
    4 0x00bfffff # deep sky blue
    5 0xffff00ff # yellow
    6 0xda70d6ff # orchid
    7 0x00ff7fff # spring green
    8 0xff8247ff # siennal
    # repeat
    # I thought the dark orange was too similar to siennal
    9 :blue # 0xff8c00ff # dark orange
   10 0xff1493ff # deep pink
   11 0x7fff00ff # chartreuse
   12 0x00bfffff # deep sky blue
   13 0xffff00ff # yellow
   14 0xda70d6ff # orchid
   15 0x00ff7fff # spring green
   16 0xff8247ff # siennal
  })

(comment
  (((((((((()))))))))))


(var depth 0)

(varfn inc-depth
  [pos]
  (++ depth)
  [pos depth :open-paren])

(varfn dec-depth
  [pos]
  (let [res [pos depth :close-paren]]
    (-= depth 1)
    res))

(def jg
  ~{:main (some :input)
    #
    :input (choice :non-form
                   :form)
    #
    :non-form (choice :whitespace
                      :comment)
    #
    :whitespace (set " \0\f\n\r\t\v")
    #
    :comment (sequence "#"
                       (any (if-not (set "\r\n") 1)))
    #
    :form (choice :reader-macro
                  :collection
                  :literal)
    #
    :reader-macro (choice :fn
                          :quasiquote
                          :quote
                          :splice
                          :unquote)
    #
    :fn (sequence "|"
                  (any :non-form)
                  :form)
    #
    :quasiquote (sequence "~"
                          (any :non-form)
                          :form)
    #
    :quote (sequence "'"
                     (any :non-form)
                     :form)
    #
    :splice (sequence ";"
                      (any :non-form)
                      :form)
    #
    :unquote (sequence ","
                       (any :non-form)
                       :form)
    #
    :literal (choice :number
                     :constant
                     :buffer
                     :string
                     :long-buffer
                     :long-string
                     :keyword
                     :symbol)
    #
    :collection (choice :array
                        :bracket-array
                        :tuple
                        :bracket-tuple
                        :table
                        :struct)
    #
    :number (drop (cmt
                    (capture (some :name-char))
                    ,scan-number))
    #
    :name-char (choice (range "09" "AZ" "az" "\x80\xFF")
                       (set "!$%&*+-./:<?=>@^_"))
    #
    :constant (sequence (choice "false" "nil" "true")
                        (not :name-char))
    #
    :buffer (sequence "@\""
                      (any (choice :escape
                                   (if-not "\"" 1)))
                      "\"")
    #
    :escape (sequence "\\"
                      (choice (set "0efnrtvz\"\\")
                              (sequence "x" [2 :hex])
                              (sequence "u" [4 :hex])
                              (sequence "U" [6 :hex])
                              (error (constant "bad escape"))))
    #
    :hex (range "09" "af" "AF")
    #
    :string (sequence "\""
                      (any (choice :escape
                                   (if-not "\"" 1)))
                      "\"")
    #
    :long-string :long-bytes
    #
    :long-bytes {:main (drop (sequence :open
                                       (any (if-not :close 1))
                                       :close))
                 :open (capture :delim :n)
                 :delim (some "`")
                 :close (cmt (sequence (not (look -1 "`"))
                                       (backref :n)
                                       (capture :delim))
                             ,=)}
    #
    :long-buffer (sequence "@"
                           :long-bytes)
    #
    :keyword (sequence ":"
                       (any :name-char))
    #
    :symbol (some :name-char)

    :ptuple (* (cmt (* ($) "(") ,inc-depth) :root (opt (cmt (* ($) ")") ,dec-depth)))
    :btuple (* (cmt (* ($) "[") ,inc-depth) :root (opt (cmt (* ($) "]") ,dec-depth)))
    :struct (* (cmt (* ($) "{") ,inc-depth) :root2 (opt (cmt (* ($) "}") ,dec-depth)))

    #
    :array #(sequence "@("
    #          (any :input)
    #          (choice ")"
    #                  (error (constant "missing )"))))
    (* (cmt (* "@" ($) "(") ,inc-depth) (any :input) (opt (cmt (* ($) ")") ,dec-depth)))

    #
    :tuple #(sequence "("
    #                 (any :input)
    #                 (choice ")"
    #                         (error (constant "missing )"))))
    (* (cmt (* ($) "(") ,inc-depth) (any :input) (opt (cmt (* ($) ")") ,dec-depth)))
    #
    :bracket-array #(sequence "@["
    #                         (any :input)
    #                         (choice "]"
    #                                 (error (constant "missing ]"))))
    (* (cmt (* "@" ($) "[") ,inc-depth) (any :input) (opt (cmt (* ($) "]") ,dec-depth)))
    #
    :bracket-tuple #(sequence "["
    #                         (any :input)
    #                         (choice "]"
    #                                 (error (constant "missing ]"))))
    (* (cmt (* ($) "[") ,inc-depth) (any :input) (opt (cmt (* ($) "]") ,dec-depth)))

    :table #(sequence "@{"
    #          (any :input)
    #          (choice "}"
    #                  (error (constant "missing }"))))
    (* (cmt (* "@" ($) "{") ,inc-depth) (any :input) (opt (cmt (* ($) "}") ,dec-depth)))
    #
    :struct #(sequence "{"
    #                  (any :input)
    #                  (choice "}"
    #                          (error (constant "missing }"))))

    (* (cmt (* ($) "{") ,inc-depth) (any :input) (opt (cmt (* ($) "}") ,dec-depth)))})

(comment

  (peg/match jg "")
  # => nil

  (peg/match jg "@\"i am a buffer\"")
  # => @[]

  (peg/match jg "# hello")
  # => @[]

  (peg/match jg "nil")
  # => @[]

  (peg/match jg ":a")
  # => @[]

  (peg/match jg "@``i am a long buffer``")
  # => @[]

  (peg/match jg "``hello``")
  # => @[]

  (peg/match jg "8")
  # => @[]

  (peg/match jg "-2.0")
  # => @[]

  (peg/match jg "\"\\u0001\"")
  # => @[]

  (peg/match jg "a")
  # => @[]

  (peg/match jg " ")
  # => @[]

  (peg/match jg "|(+ $ 2)")
  # => @[]

  (peg/match jg "~a")
  # => @[]

  (peg/match jg "'a")
  # => @[]

  (peg/match jg ";a")
  # => @[]

  (peg/match jg ",a")
  # => @[]

  (peg/match jg "@(:a)")
  # => @[]

  (peg/match jg "@[:a]")
  # => @[]

  (peg/match jg "[:a]")
  # => @[]

  (peg/match jg "[1 2]")
  # => @[]

  (peg/match jg "@{:a 1}")
  # => @[]

  (peg/match jg "{:a 1}")
  # => @[]

  (peg/match jg "(:a)")
  # => @[]

  (peg/match jg "(def a 1)")
  # => @[]

  (peg/match jg "[:a :b] 1")
  # => @[]

  (try
    (peg/match jg "[:a :b)")
    ([e] e))
  # => "missing ]"

  (try
    (peg/match jg "(def a # hi 1)")
    ([e] e))
  # => "missing )"

  (try
    (peg/match jg "\"\\u001\"")
    ([e] e))
  # => "bad escape"
)

(def delims-pos-grammar
  ~{:ws (set " \t\r\f\n\0\v")
    :readermac (set "';~,|")
    :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_"))
    :token (some :symchars)
    :hex (range "09" "af" "AF")
    :escape (* "\\" (+ (set "ntrzfev0\"\\")
                       (* "x" :hex :hex)
                       (* "u" [4 :hex])
                       (* "U" [6 :hex])
                       #(error (constant "bad escape"))
))
    :comment (* "#" (any (if-not (+ "\n" -1) 1)))
    :symbol :token
    :keyword (* ":" (any :symchars))
    :constant (* (+ "true" "false" "nil") (not :symchars))
    :bytes (* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
    :string :bytes
    :buffer (* "@" :bytes)
    :long-bytes {:delim (some "`")
                 :open (capture :delim :n)
                 :close (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=)
                 :main (drop (* :open (any (if-not :close 1)) :close))}
    :long-string :long-bytes
    :long-buffer (* "@" :long-bytes)
    :number (drop (cmt (<- :token) ,scan-number))
    :raw-value (+ :comment :constant :number :keyword
                  :string :buffer :long-string :long-buffer
                  :parray :barray :ptuple :btuple :struct :dict :symbol)
    :value (* (any (+ :ws :readermac)) :raw-value (any :ws))
    :root (any :value)
    :root2 (any (* :value :value))
    :ptuple (* (cmt (* ($) "(") ,inc-depth) :root (opt (cmt (* ($) ")") ,dec-depth)))
    :btuple (* (cmt (* ($) "[") ,inc-depth) :root (opt (cmt (* ($) "]") ,dec-depth)))
    :struct (* (cmt (* ($) "{") ,inc-depth) :root2 (opt (cmt (* ($) "}") ,dec-depth)))
    :parray (* "@" :ptuple)
    :barray (* "@" :btuple)
    :dict (* "@" :struct)
    :main :root})

(comment
  (pp (peg/match delims-pos-grammar
                 ``
(+ 1 (* 3 3))
# hej )
(* 5 # 123 )
)
``))
  #
)

(varfn gb->delim-ps
  [gb]
  (set depth 0)
  (peg/match jg # delims-pos-grammar
 (content gb)))
