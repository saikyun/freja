(use ./new_gap_buffer)

(defn rgba->f
  [r g b & [a]]
  [(/ r 255)
   (/ g 255)
   (/ b 255)
   (or a 1)])

(def colors
  @{1 (rgba->f 120 11 151)
    2 0x77cc77ff
    3 :dark-blue
    4 0xee99ddff
    5 0x228822ff
    6 :sky-blue
    7 :dark-purple
    8 0x77cc77ff
    9 :dark-blue
    10 0xee99ddff
    11 0x228822ff
    12 :sky-blue
    13 :dark-purple})


(comment
(((((((((())))))))))
)


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

(pp (peg/match delims-pos-grammar
               ``
(+ 1 (* 3 3))
# hej )
(* 5 # 123 )
)
``))

(varfn gb->delim-ps
  [gb]
  (set depth 0)
  (peg/match delims-pos-grammar (content gb)))
