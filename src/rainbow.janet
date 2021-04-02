(use ./new_gap_buffer)

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

(defn rgba->f
  [r g b & [a]]
  [(/ r 255)
   (/ g 255)
   (/ b 255)
   (or a 1)])

(def colors
  @{1 (rgba->f 180 21 191)
    2 (rgba->f 42 143 39)
    3 (rgba->f 82 144 199)
    4 (rgba->f 72 184 87)
    5 (rgba->f 186 104 172)
    6 (rgba->f 21 191 186)
    7 (rgba->f 140 1 151)
    8 (rgba->f 2 103 9)
    9 (rgba->f 42 104 159)
    10 (rgba->f 32 144 47)
    11 (rgba->f 146 64 132)
    12 (rgba->f 1 151 146)})

(varfn gb->delim-ps
  [gb]
  (set depth 0)
  (peg/match delims-pos-grammar (content gb)))
