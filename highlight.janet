(defn styling
  [k patt]
  ['/ ['* ['$] patt ['$]]
   (fn [start stop]
     {:kind k :start start :stop stop})])

(var sexp-grammar
  ~{:ws (set " \t\r\f\n\0\v")
    :readermac (set "';~,|")
    :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_"))
    :token (some :symchars)
    :hex (range "09" "af" "AF")
    :escape (* "\\" (+ (set "ntrzfev0\"\\")
                      (* "x" :hex :hex)
                      (* "u" [4 :hex])
                      (* "U" [6 :hex])
                      (error (constant "bad escape"))))
    :comment (* "#" (any (if-not (+ "\n" -1) 1)))
    :special-symbol (choice "defn" "var" "set")
    :symbol :token
    :keyword (* ":" (any :symchars))
    :constant (* (+ "true" "false" "nil") (not :symchars))
    :bytes (* "\"" (any (+ :escape (if-not "\"" 1))) (opt "\""))
    :string :bytes
    :buffer (* "@" :bytes)
    :long-bytes {:delim (some "`")
                 :open (capture :delim :n)
                 :close (drop (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=))
                 :main (drop (* :open (any (if-not :close 1)) :close))}
    :long-string :long-bytes
    :long-buffer (* "@" :long-bytes)
    :number (drop (cmt (<- :token) ,scan-number))
    :raw-value (+ :comment :constant :number :keyword
                 :string :buffer :long-string :long-buffer
                 :parray :barray :ptuple :btuple :struct :dict :special-symbol :symbol)
    :value (* (any (+ :ws :readermac)) :raw-value (any :ws))
    :root (any (<- :value))
    :root2 (any (* :value :value))
    :ptuple (* "(" :root (+ ")" (error "")))
    :btuple (* "[" :root (+ "]" (error "")))
    :struct (* "{" :root2 (+ "}" (error "")))
    :parray (* "@" :ptuple)
    :barray (* "@" :btuple)
    :dict (* "@" :struct)
    :main :root})

(var styling-grammar
  ~{:ws (set " \t\r\f\n\0\v")
    :readermac (set "';~,|")
    :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_"))
    :token (some :symchars)
    :hex (range "09" "af" "AF")
    :escape (* "\\" (+ (set "ntrzfev0\"\\")
                      (* "x" :hex :hex)
                      (* "u" [4 :hex])
                      (* "U" [6 :hex])
                      (error (constant "bad escape"))))
    :comment ,(styling :comment ~(* "#" (any (if-not (+ "\n" -1) 1))))
    :special-symbol ,(styling :special-symbol '(choice "defn" "var" "set"))
    :symbol ,(styling :symbol :token)
    :keyword ,(styling :keyword ~(* ":" (any :symchars)))
    :constant (* (+ "true" "false" "nil") (not :symchars))
    :bytes (* "\"" (any (+ :escape (if-not "\"" 1))) (opt "\""))
    :string ,(styling :string :bytes)
    :buffer ,(styling :string ~(* "@" :bytes))
    :long-bytes {:delim (some "`")
                 :open (capture :delim :n)
                 :close (drop (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=))
                 :main (drop (* :open (any (if-not :close 1)) :close))}
    :long-string ,(styling :long-string :long-bytes)
    :long-buffer ,(styling :long-string ~(* "@" :long-bytes))
    :number ,(styling :number ~(drop (cmt (<- :token) ,scan-number)))
    :raw-value (+ :comment :constant :number :keyword
                 :string :buffer :long-string :long-buffer
                 :parray :barray :ptuple :btuple :struct :dict :special-symbol :symbol)
    :value (* (any (+ :ws :readermac)) :raw-value (any :ws))
    :root (any :value)
    :root2 (any (* :value :value))
    :ptuple (* "(" :root (opt ")"))
    :btuple (* "[" :root (opt "]"))
    :struct (* "{" :root2 (opt "}"))
    :parray (* "@" :ptuple)
    :barray (* "@" :btuple)
    :dict (* "@" :struct)
    :main :root})
