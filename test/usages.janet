# janet-usages

# includes various portions of (or inspiration from) bakpakin's:
#
# * helper.janet
# * jpm
# * path.janet
# * peg for janet

### path.janet
###
### A library for path manipulation.
###
### Copyright 2019 © Calvin Rose

# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#
# Common
#

(def- path/ext-peg
  (peg/compile ~{:back (> -1 (+ (* ($) (set "\\/.")) :back))
                 :main :back}))

(defn path/ext
  "Get the file extension for a path."
  [path]
  (if-let [m (peg/match path/ext-peg path (length path))]
    (let [i (m 0)]
      (if (= (path i) 46)
        (string/slice path (m 0) -1)))))

(defn- path/redef
  "Redef a value, keeping all metadata."
  [from to]
  (setdyn (symbol to) (dyn (symbol from))))

#
# Generating Macros
#

(defmacro- path/decl-sep [pre sep] ~(def ,(symbol pre "/sep") ,sep))
(defmacro- path/decl-delim [pre d] ~(def ,(symbol pre "/delim") ,d))

(defmacro- path/decl-last-sep
  [pre sep]
  ~(def- ,(symbol pre "/last-sep-peg")
    (peg/compile '{:back (> -1 (+ (* ,sep ($)) :back))
                   :main (+ :back (constant 0))})))

(defmacro- path/decl-dirname
  [pre]
  ~(defn ,(symbol pre "/dirname")
     "Gets the directory name of a path."
     [path]
     (if-let [m (peg/match
                  ,(symbol pre "/last-sep-peg")
                  path
                  (length path))]
       (let [[p] m]
         (if (zero? p) "./" (string/slice path 0 p)))
       path)))

(defmacro- path/decl-basename
  [pre]
  ~(defn ,(symbol pre "/basename")
     "Gets the base file name of a path."
     [path]
     (if-let [m (peg/match
                  ,(symbol pre "/last-sep-peg")
                  path
                  (length path))]
       (let [[p] m]
         (string/slice path p -1))
       path)))

(defmacro- path/decl-parts
  [pre sep]
  ~(defn ,(symbol pre "/parts")
     "Split a path into its parts."
     [path]
     (string/split ,sep path)))

(defmacro- path/decl-normalize
  [pre sep sep-pattern lead]
  (defn capture-lead
    [& xs]
    [:lead (xs 0)])
  (def grammar
    ~{:span (some (if-not ,sep-pattern 1))
      :sep (some ,sep-pattern)
      :main (* (? (* (replace ',lead ,capture-lead) (any ,sep-pattern)))
               (? ':span)
               (any (* :sep ':span))
               (? (* :sep (constant ""))))})
  (def peg (peg/compile grammar))
  ~(defn ,(symbol pre "/normalize")
     "Normalize a path. This removes . and .. in the
     path, as well as empty path elements."
     [path]
     (def accum @[])
     (def parts (peg/match ,peg path))
     (var seen 0)
     (var lead nil)
     (each x parts
       (match x
         [:lead what] (set lead what)
         "." nil
         ".." (if (= 0 seen)
                (array/push accum x)
                (do (-- seen) (array/pop accum)))
         (do (++ seen) (array/push accum x))))
     (def ret (string (or lead "") (string/join accum ,sep)))
     (if (= "" ret) "." ret)))

(defmacro- path/decl-join
  [pre sep]
  ~(defn ,(symbol pre "/join")
     "Join path elements together."
     [& els]
     (,(symbol pre "/normalize") (string/join els ,sep))))

(defmacro- path/decl-abspath
  [pre]
  ~(defn ,(symbol pre "/abspath")
     "Coerce a path to be absolute."
     [path]
     (if (,(symbol pre "/abspath?") path)
       (,(symbol pre "/normalize") path)
       (,(symbol pre "/join") (or (dyn :path-cwd) (os/cwd)) path))))

#
# Posix
#

(defn path/posix/abspath?
  "Check if a path is absolute."
  [path]
  (string/has-prefix? "/" path))

(path/redef "path/ext" "path/posix/ext")
(path/decl-sep "path/posix" "/")
(path/decl-delim "path/posix" ":")
(path/decl-last-sep "path/posix" "/")
(path/decl-basename "path/posix")
(path/decl-dirname "path/posix")
(path/decl-parts "path/posix" "/")
(path/decl-normalize "path/posix" "/" "/" "/")
(path/decl-join "path/posix" "/")
(path/decl-abspath "path/posix")

#
# Windows
#

(def- path/abs-pat '(* (? (* (range "AZ" "az") `:`)) `\`))
(def- path/abs-peg (peg/compile path/abs-pat))
(defn path/win32/abspath?
  "Check if a path is absolute."
  [path]
  (not (not (peg/match path/abs-peg path))))

(path/redef "path/ext" "path/win32/ext")
(path/decl-sep "path/win32" "\\")
(path/decl-delim "path/win32" ";")
(path/decl-last-sep "path/win32" "\\")
(path/decl-basename "path/win32")
(path/decl-dirname "path/win32")
(path/decl-parts "path/win32" "\\")
(path/decl-normalize "path/win32" `\` (set `\/`) (* (? (* (range "AZ" "az") `:`)) `\`))
(path/decl-join "path/win32" "\\")
(path/decl-abspath "path/win32")

#
# Satisfy linter
#

(defn path/sep [pre sep] nil)
(defn path/delim [pre d] nil)
(defn path/dirname [pre] nil)
(defn path/basename [pre] nil)
(defn path/parts [pre sep] nil)
(defn path/normalize [pre sep sep-pattern lead] nil)
(defn path/join [pre sep] nil)
(defn path/abspath [pre] nil)
(defn path/abspath? [path] nil)

#
# Specialize for current OS
#

(def- path/syms
  ["ext"
   "sep"
   "delim"
   "basename"
   "dirname"
   "abspath?"
   "abspath"
   "parts"
   "normalize"
   "join"])
(let [pre (if (= :windows (os/which)) "path/win32" "path/posix")]
  (each sym path/syms
    (path/redef (string pre "/" sym) (string "path/" sym))))

(defn display/print-color
  [msg color]
  # XXX: what if color doesn't match...
  (let [color-num (match color
                    :black 30
                    :blue 34
                    :cyan 36
                    :green 32
                    :magenta 35
                    :red 31
                    :white 37
                    :yellow 33)]
    (prin (string "\e[" color-num "m"
                  msg
                  "\e[0m"))))

(defn display/dashes
  [&opt n]
  (default n 60)
  (string/repeat "-" n))

(defn display/print-dashes
  [&opt n]
  (print (display/dashes n)))

(defn display/print-form
  [form &opt color]
  (def buf @"")
  (with-dyns [:out buf]
    (printf "%m" form))
  (def msg (string/trimr buf))
  (print ":")
  (if color
    (display/print-color msg color)
    (prin msg))
  (print))
# some bits from jpm

### Copyright 2019 © Calvin Rose

# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

(def- jpm/is-win (= (os/which) :windows))
(def- jpm/is-mac (= (os/which) :macos))
(def- jpm/sep (if jpm/is-win "\\" "/"))

(defn jpm/rm
  "Remove a directory and all sub directories."
  [path]
  (case (os/lstat path :mode)
    :directory (do
      (each subpath (os/dir path)
        (jpm/rm (string path jpm/sep subpath)))
      (os/rmdir path))
    nil nil # do nothing if file does not exist
    # Default, try to remove
    (os/rm path)))

(def- jpm/path-splitter
  "split paths on / and \\."
  (peg/compile ~(any (* '(any (if-not (set `\/`) 1)) (+ (set `\/`) -1)))))

(defn jpm/shell
  "Do a shell command"
  [& args]
  (if (dyn :verbose)
    (print ;(interpose " " args)))
  (os/execute args :px))

(defn jpm/copy
  "Copy a file or directory recursively from one location to another."
  [src dest]
  (print "copying " src " to " dest "...")
  (if jpm/is-win
    (let [end (last (peg/match jpm/path-splitter src))
          isdir (= (os/stat src :mode) :directory)]
      (jpm/shell "C:\\Windows\\System32\\xcopy.exe"
                 (string/replace "/" "\\" src)
                 (string/replace "/" "\\" (if isdir (string dest "\\" end) dest))
                 "/y" "/s" "/e" "/i"))
    (jpm/shell "cp" "-rf" src dest)))

(defn jpm/create-dirs
  "Create all directories needed for a file (mkdir -p)."
  [dest]
  (def segs (peg/match jpm/path-splitter dest))
  (for i 1 (length segs)
    (def path (string/join (slice segs 0 i) jpm/sep))
    (unless (empty? path) (os/mkdir path))))

(defn jpm/copy-continue
  "Copy a file or directory recursively from one location to another."
  [src dest]
  (print "copying " src " to " dest "...")
  (if jpm/is-win
    (let [end (last (peg/match jpm/path-splitter src))
          isdir (= (os/stat src :mode) :directory)]
      (jpm/shell "C:\\Windows\\System32\\xcopy.exe"
                 (string/replace "/" "\\" src)
                 (string/replace "/" "\\" (if isdir (string dest "\\" end) dest))
                 "/y" "/s" "/e" "/i" "/c"))
    (jpm/shell "cp" "-rf" src dest)))
(defn input/slurp-input
  [input]
  (var f nil)
  (try
    (if (= input "-")
      (set f stdin)
      (if (os/stat input)
        (set f (file/open input :rb))
        (do
          (eprint "path not found: " input)
          (break nil))))
    ([err]
      (eprintf "slurp-input failed")
      (error err)))
  #
  (var buf nil)
  (defer (file/close f)
    (set buf @"")
    (file/read f :all buf))
  buf)
(def name/prog-name
  "janet-usages")

(def name/dot-dir-name
  ".janet-usages")
# adapted from:
#   https://janet-lang.org/docs/syntax.html

# approximation of janet's grammar
(def grammar/janet
  ~{:main :root
    #
    :root (any :root0)
    #
    :root0 (choice :value :comment)
    #
    :value (sequence
            (any (choice :s :readermac))
            :raw-value
            (any :s))
    #
    :readermac (set "',;|~")
    #
    :raw-value (choice
                :string :buffer
                :long-string :long-buffer
                :parray :barray
                :ptuple :btuple
                :struct :table
                :constant :number
                :symbol :keyword)
    #
    :comment (sequence (any :s)
                       "#"
                       (any (if-not (choice "\n" -1) 1))
                       (any :s))
    #
    :constant (choice "false" "nil" "true")
    #
    :number (drop (cmt
                   (capture :token)
                   ,scan-number))
    #
    :token (some :symchars)
    #
    :symchars (choice
               (range "09" "AZ" "az" "\x80\xFF")
               # XXX: see parse.c's is_symbol_char which mentions:
               #
               #        \, ~, and |
               #
               #      but tools/symcharsgen.c does not...
               (set "!$%&*+-./:<?=>@^_"))
    #
    :keyword (sequence ":" (any :symchars))
    #
    :string :bytes
    #
    :bytes (sequence "\""
                     (any (choice :escape (if-not "\"" 1)))
                     "\"")
    #
    :escape (sequence "\\"
                      (choice (set "0efnrtvz\"\\")
                              (sequence "x" [2 :hex])
                              (sequence "u" [4 :d])
                              (sequence "U" [6 :d])
                              (error (constant "bad escape"))))
    #
    :hex (range "09" "af" "AF")
    #
    :buffer (sequence "@" :bytes)
    #
    :long-string :long-bytes
    #
    :long-bytes {:main (drop (sequence
                              :open
                              (any (if-not :close 1))
                              :close))
                 :open (capture :delim :n)
                 :delim (some "`")
                 :close (cmt (sequence
                              (not (look -1 "`"))
                              (backref :n)
                              (capture :delim))
                             ,=)}
    #
    :long-buffer (sequence "@" :long-bytes)
    #
    :parray (sequence "@" :ptuple)
    #
    :ptuple (sequence "("
                      :root
                      (choice ")" (error "")))
    #
    :barray (sequence "@" :btuple)
    #
    :btuple (sequence "["
                      :root
                      (choice "]" (error "")))
    # XXX: constraining to an even number of values doesn't seem
    #      worth the work when considering that comments can also
    #      appear in a variety of locations...
    :struct (sequence "{"
                      :root
                      (choice "}" (error "")))
    #
    :table (sequence "@" :struct)
    #
    :symbol :token
    })

(comment

  (try
    (peg/match grammar/janet "\"\\u001\"")
    ([e] e))
  # => "bad escape"

  (peg/match grammar/janet "\"\\u0001\"")
  # => @[]

  (peg/match grammar/janet "(def a 1)")
  # => @[]

  (try
    (peg/match grammar/janet "[:a :b)")
    ([e] e))
  # => "match error at line 1, column 7"

  (peg/match grammar/janet "(def a # hi\n 1)")
  # => @[]

  (try
    (peg/match grammar/janet "(def a # hi 1)")
    ([e] e))
  # => "match error at line 1, column 15"

  (peg/match grammar/janet "[1]")
  # => @[]

  (peg/match grammar/janet "# hello")
  # => @[]

  (peg/match grammar/janet "``hello``")
  # => @[]

  (peg/match grammar/janet "8")
  # => @[]

  (peg/match grammar/janet "[:a :b]")
  # => @[]

  (peg/match grammar/janet "[:a :b] 1")
  # => @[]

 )
(defn validate/valid-code?
  [form-bytes]
  (let [p (parser/new)
        p-len (parser/consume p form-bytes)]
    (when (parser/error p)
      (break false))
    (parser/eof p)
    (and (= (length form-bytes) p-len)
         (nil? (parser/error p)))))

(comment

  (validate/valid-code? "true")
  # => true

  (validate/valid-code? "(")
  # => false

  (validate/valid-code? "()")
  # => true

  (validate/valid-code? "(]")
  # => false

  )

# XXX: any way to avoid this?
(var- pegs/topish-level 0)

(defn- pegs/track-top-level-peg
  [l-delim r-delim]
  ~(sequence (drop (cmt (capture ,l-delim)
                        ,|(do
                            (++ pegs/topish-level)
                            $)))
             :root
             (choice (drop (cmt (capture ,r-delim)
                                ,|(do
                                    (-- pegs/topish-level)
                                    $)))
                     (error ""))))

(def- pegs/comment-analyzer
  (->
    # grammar/janet is a struct, need something mutable
    (table ;(kvs grammar/janet))
    (put :main '(choice (capture :value)
                        :comment))
    # tracking of "top-level"-ness (within a `comment`)
    (put :ptuple
         (pegs/track-top-level-peg "(" ")"))
    (put :btuple
         (pegs/track-top-level-peg "[" "]"))
    (put :struct
         (pegs/track-top-level-peg "{" "}"))
    # classify certain comments
    (put :comment
         ~(sequence
            (any :s)
            (choice
              (cmt (sequence
                     (line)
                     "#" (any :s) "=>"
                     (capture (sequence
                                (any (if-not (choice "\n" -1) 1))
                                (any "\n"))))
                   ,|(if (zero? pegs/topish-level)
                       (let [ev-form (string/trim $1)
                             line $0]
                         (assert (validate/valid-code? ev-form)
                                 {:ev-form ev-form
                                  :line line})
                         # record expected value form and line
                         [:returns ev-form line])
                       # XXX: is this right?
                       ""))
              (cmt (capture (sequence
                              "#"
                              (any (if-not (+ "\n" -1) 1))
                              (any "\n")))
                   ,|(if (zero? pegs/topish-level)
                       (identity $)
                       # XXX: is this right?
                       "")))
            (any :s)))
    # tried using a table with a peg but had a problem, so use a struct
    table/to-struct))

(def pegs/inner-forms
  ~(sequence "("
             (any :s)
             "comment"
             (any :s)
             (any (choice :s ,pegs/comment-analyzer))
             (any :s)
             ")"))

(comment

  (deep=
    #
    (peg/match
      pegs/inner-forms
      ``
      (comment
        (- 1 1)
        # => 0
      )
      ``)
    #
    @["(- 1 1)\n  "
      [:returns "0" 3]])
  # => true

  (deep=
    #
    (peg/match
      pegs/inner-forms
      ``
      (comment

        (def a 1)

        # this is just a comment

        (def b 2)

        (= 1 (- b a))
        # => true

      )
      ``)
    #
    @["(def a 1)\n\n  "
      "# this is just a comment\n\n"
      "(def b 2)\n\n  "
      "(= 1 (- b a))\n  "
      [:returns "true" 10]])
  # => true

  # demo of having failure test output give nicer results
  (def result
    @["(def a 1)\n\n  "
      "# this is just a comment\n\n"
      "(def b 2)\n\n  "
      "(= 1 (- b a))\n  "
      [:returns "true" 10]])

  (peg/match
    pegs/inner-forms
    ``
    (comment

      (def a 1)

      # this is just a comment

      (def b 2)

      (= 1 (- b a))
      # => true

    )
    ``)
  # => result

  # thanks Saikyun
  (peg/match
    pegs/inner-forms
    ``
    (comment

      @{:bye 10 #hello
       }

      (+ 1 1)
      # => 2

    )
    ``)
  # => '@["" "@{:bye 10 #hello\n   }\n\n  " "(+ 1 1)\n  " (:returns "2" 7)]

  )

(defn pegs/parse-comment-block
  [cmt-blk-str]
  # mutating outer pegs/topish-level
  (set pegs/topish-level 0)
  (peg/match pegs/inner-forms cmt-blk-str))

(comment

  (def comment-str
    ``
    (comment

      (+ 1 1)
      # => 2

    )
    ``)

  (pegs/parse-comment-block comment-str)
  # => @["(+ 1 1)\n  " [:returns "2" 4]]

  (def comment-with-no-test-str
    ``
    (comment

      (+ 1 1)

    )
    ``)

  (pegs/parse-comment-block comment-with-no-test-str)
  # => @["(+ 1 1)\n\n"]

  (def comment-in-comment-str
    ``
    (comment

      (comment

         (+ 1 1)
         # => 2

       )
    )
    ``)

  (pegs/parse-comment-block comment-in-comment-str)
  # => @["" "(comment\n\n     (+ 1 1)\n     # => 2\n\n   )\n"]

  # thanks Saikyun
  (def comment-in-struct-str
    ``
    (comment

      @{:bye 10 #hello
       }

      (+ 1 1)
      # => 2

    )
    ``)

  (pegs/parse-comment-block comment-in-struct-str)
  # => '@["" "@{:bye 10 #hello\n   }\n\n  " "(+ 1 1)\n  " (:returns "2" 7)]

  )

# recognize next top-level form, returning a map
# modify a copy of grammar/janet
(def pegs/top-level
  (->
    # grammar/janet is a struct, need something mutable
    (table ;(kvs grammar/janet))
    # also record location and type information, instead of just recognizing
    (put :main ~(choice (cmt (sequence
                               (line)
                               (capture :value)
                               (position))
                             ,|(do
                                 (def [s-line value end] $&)
                                 {:end end
                                  :s-line s-line
                                  :type :value
                                  :value value}))
                        (cmt (sequence
                               (line)
                               (capture :comment)
                               (position))
                             ,|(do
                                 (def [s-line value end] $&)
                                 {:end end
                                  :s-line s-line
                                  :type :comment
                                  :value value}))))
    # tried using a table with a peg but had a problem, so use a struct
    table/to-struct))

(comment

  (def sample-source
    (string "# \"my test\"\n"
            "(+ 1 1)\n"
            "# => 2\n"))

  (deep=
    #
    (peg/match pegs/top-level sample-source 0)
    #
    @[{:type :comment
       :value "# \"my test\"\n"
       :s-line 1
       :end 12}]) # => true

  (def result
    @[{:type :value
       :value "(+ 1 1)\n"
       :s-line 2
       :end 20}])

  (peg/match pegs/top-level sample-source 12)
  # => result

  (string/slice sample-source 12 20)
  # => "(+ 1 1)\n"

  (deep=
    #
    (peg/match pegs/top-level sample-source 20)
    #
    @[{:type :comment
       :value "# => 2\n"
       :s-line 3
       :end 27}]) # => true

  )

(comment

  (def top-level-comments-sample
    ``
    (def a 1)

    (comment

      (+ 1 1)

      # hi there

      (comment :a )

    )

    (def x 0)

    (comment

      (= a (+ x 1))

    )
    ``)

  (deep=
    #
    (peg/match pegs/top-level top-level-comments-sample)
    #
    @[{:type :value
       :value "(def a 1)\n\n"
       :s-line 1
       :end 11}]
    ) # => true

  (deep=
    #
    (peg/match pegs/top-level top-level-comments-sample 11)
    #
    @[{:type :value
       :value
       "(comment\n\n  (+ 1 1)\n\n  # hi there\n\n  (comment :a )\n\n)\n\n"
       :s-line 3
       :end 66}]
    ) # => true

  (deep=
    #
    (peg/match pegs/top-level top-level-comments-sample 66)
    #
    @[{:type :value
       :value "(def x 0)\n\n"
       :s-line 13
       :end 77}]
    ) # => true

  (deep=
    #
    (peg/match pegs/top-level top-level-comments-sample 77)
    #
    @[{:type :value
       :value "(comment\n\n  (= a (+ x 1))\n\n)"
       :s-line 15
       :end 105}]
    ) # => true

  )

# XXX: not quite right, but good enough?
(def pegs/comment-block-maybe
  ~(sequence (any :s)
             "("
             (any :s)
             "comment"
             (any :s)))

(comment

  (peg/match
    pegs/comment-block-maybe
    ``
    (comment

      (= a (+ x 1))

    )
    ``)
  # => @[]

  (peg/match
    pegs/comment-block-maybe
    ``

    (comment

      :a
    )
    ``)
  # => @[]

  )

# XXX: simplify?
(defn rewrite/rewrite-tagged
  [tagged-item last-form offset]
  (match tagged-item
    [:returns value line]
    (string "(_verify/is "
            last-form " "
            value " "
            (string "\"" "line-" (dec (+ line offset)) "\"") ")\n\n")
    nil))

(comment

  (rewrite/rewrite-tagged [:returns true 1] "(= 1 1)" 1)
  # => "(_verify/is (= 1 1) true \"line-1\")\n\n"

  )

# XXX: tried putting the following into a file, but kept having
#      difficulty getting it to work out
# XXX: an advantage of it being in a separate file is that testing
#      the contained code might be easier...
(def rewrite/verify-as-string
  ``
  # influenced by janet's tools/helper.janet

  (var _verify/start-time 0)
  (var _verify/end-time 0)
  (var _verify/test-results @[])

  (defmacro _verify/is
    [t-form e-form &opt name]
    (default name
      (string "test-" (inc (length _verify/test-results))))
    (with-syms [$ts $tr
                $es $er]
      ~(do
         (def [,$ts ,$tr] (protect ,t-form))
         (def [,$es ,$er] (protect ,e-form))
         (array/push _verify/test-results
                     {:expected-form ',e-form
                      :expected-value ,$er
                      :name ,name
                      :passed (if (and ,$ts ,$es)
                                (deep= ,$tr ,$er)
                                nil)
                      :test-form ',t-form
                      :test-value ,$tr
                      :type :is})
         ,name)))

  (defn _verify/start-tests
    []
    (set _verify/start-time (os/clock))
    (set _verify/test-results @[]))

  (defn _verify/end-tests
    []
    (set _verify/end-time (os/clock)))

  (defn _verify/dump-results
    []
    (if-let [test-out (dyn :judge-gen/test-out)]
      (spit test-out (marshal _verify/test-results))
      # XXX: could this sometimes have problems?
      (printf "%p" _verify/test-results)))

  ``)

(defn rewrite/has-tests?
  [forms]
  (when forms
    (some |(tuple? $)
          forms)))

(comment

  (rewrite/has-tests? @["(+ 1 1)\n  " [:returns "2" 1]])
  # => true

  (rewrite/has-tests? @["(comment \"2\")\n  "])
  # => nil

  )

(defn rewrite/rewrite-block-with-verify
  [blk]
  (def rewritten-forms @[])
  (def {:value blk-str
        :s-line offset} blk)
  # parse the comment block and rewrite some parts
  (let [parsed (try
                 (pegs/parse-comment-block blk-str)
                 ([err]
                   (error (merge err {:offset offset}))))]
    (when (rewrite/has-tests? parsed)
      (var just-saw-ev false)
      (each cmt-or-frm parsed
        (when (not= cmt-or-frm "")
          (if (empty? rewritten-forms)
            (array/push rewritten-forms cmt-or-frm)
            # is `cmt-or-frm` an expected value
            (if (= (type cmt-or-frm) :tuple)
              # looks like an expected value, may be rewrite as test
              (let [last-form (array/pop rewritten-forms)
                    rewritten (rewrite/rewrite-tagged cmt-or-frm
                                                      last-form offset)]
                (assert (not just-saw-ev)
                        (string/format
                          "unexpected expected value comment beyond line: %d"
                          offset))
                (assert rewritten
                        (string "failed to rewrite expected value: "
                                cmt-or-frm))
                (set just-saw-ev true)
                (array/push rewritten-forms rewritten))
              # not an expected value, continue
              (do
                (set just-saw-ev false)
                (array/push rewritten-forms cmt-or-frm))))))))
  rewritten-forms)

(comment

  (def comment-str
    ``
    (comment

      (+ 1 1)
      # => 2

    )
    ``)

  (def comment-blk
    {:value comment-str
     :s-line 3})

  (rewrite/rewrite-block-with-verify comment-blk)
  # => @["(_verify/is (+ 1 1)\n   2 \"line-6\")\n\n"]

  (def comment-with-no-test-str
    ``
    (comment

      (+ 1 1)

    )
    ``)

  (def comment-blk-with-no-test-str
    {:value comment-with-no-test-str
     :s-line 1})

  (rewrite/rewrite-block-with-verify comment-blk-with-no-test-str)
  # => @[]

  # comment block in comment block shields inner content
  (def comment-in-comment-str
    ``
    (comment

      (comment

         (+ 1 1)
         # => 2

       )
    )
    ``)

  (def comment-blk-in-comment-blk
    {:value comment-in-comment-str
     :s-line 10})

  (rewrite/rewrite-block-with-verify comment-blk-in-comment-blk)
  # => @[]

  )

(defn rewrite/rewrite-with-verify
  [cmt-blks]
  (var rewritten-forms @[])
  # parse comment blocks and rewrite some parts
  (each blk cmt-blks
    (array/concat rewritten-forms (rewrite/rewrite-block-with-verify blk)))
  # assemble pieces
  (var forms
    (array/concat @[]
                  @["\n\n"
                    "(_verify/start-tests)\n\n"]
                  rewritten-forms
                  @["\n(_verify/end-tests)\n"
                    "\n(_verify/dump-results)\n"]))
  (string rewrite/verify-as-string
          (string/join forms "")))

# since there are no tests in this comment block, nothing will execute
(comment

  # XXX: expected values are all large here -- not testing

  (def sample
    ``
    (comment

      (= 1 1)
      # => true

    )
    ``)

  (rewrite/rewrite-with-verify [{:value sample
                                 :s-line 1}])

  (def sample-comment-form
    ``
    (comment

      (def a 1)

      # this is just a comment

      (def b 2)

      (= 1 (- b a))
      # => true

    )
    ``)

  (rewrite/rewrite-with-verify [{:value sample-comment-form
                                 :s-line 1}])

  (def comment-in-comment
    ``
    (comment

      (comment

        (+ 1 1)
        # => 2

      )

    )
    ``)

  (rewrite/ewrite-with-verify [{:value comment-in-comment
                                :s-line 1}])

  )

(defn segments/parse
  [buf]
  (var segments @[])
  (var from 0)
  (loop [parsed :iterate (peg/match pegs/top-level buf from)]
    (when (dyn :debug)
      (eprintf "parsed: %j" parsed))
    (when (not parsed)
      (break nil))
    (def segment (first parsed))
    (when (not segment)
      (eprint "Unexpectedly did not find segment in: " parsed)
      (break nil))
    (array/push segments segment)
    (set from (segment :end)))
  segments)

(comment

  (def code-buf
    @``
     (def a 1)

     (comment

       (+ a 1)
       # => 2

       (def b 3)

       (- b a)
       # => 2

     )
     ``)

  (deep=
    (segments/parse code-buf)
    #
    @[{:value "(def a 1)\n\n"
       :s-line 1
       :type :value
       :end 11}
      {:value (string "(comment\n\n  "
                      "(+ a 1)\n  "
                      "# => 2\n\n  "
                      "(def b 3)\n\n  "
                      "(- b a)\n  "
                      "# => 2\n\n)")
       :s-line 3
       :type :value
       :end 75}]
    ) # => true

  )

(defn segments/find-comment-blocks
  [segments]
  (var comment-blocks @[])
  (loop [i :range [0 (length segments)]]
    (def segment (get segments i))
    (def {:value code-str} segment)
    (when (peg/match pegs/comment-block-maybe code-str)
      (array/push comment-blocks segment)))
  comment-blocks)

(comment

  (def segments
    @[{:value "    (def a 1)\n\n    "
       :s-line 1
       :type :value
       :end 19}
      {:value (string "(comment\n\n      "
                      "(+ a 1)\n      "
                      "# => 2\n\n      "
                      "(def b 3)\n\n      "
                      "(- b a)\n      "
                      "# => 2\n\n    "
                      ")\n    ")
       :s-line 3
       :type :value
       :end 112}])

  (deep=
    (segments/find-comment-blocks segments)
    #
    @[{:value (string "(comment\n\n      "
                      "(+ a 1)\n      "
                      "# => 2\n\n      "
                      "(def b 3)\n\n      "
                      "(- b a)\n      "
                      "# => 2\n\n    "
                      ")\n    ")
       :s-line 3
       :type :value
       :end 112}]
    )
  # => true

  )

(defn generate/handle-one
  [opts]
  (def {:input input
        :output output} opts)
  # read in the code
  (def buf (input/slurp-input input))
  (when (not buf)
    (eprint)
    (eprint "Failed to read input for: " input)
    (break false))
  # light sanity check
  (when (not (validate/valid-code? buf))
    (eprint)
    (eprint "Failed to parse input as valid Janet code: " input)
    (break false))
  # slice the code up into segments
  (def segments (segments/parse buf))
  (when (not segments)
    (eprint)
    (eprint "Failed to find segments: " input)
    (break false))
  # find comment blocks
  (def comment-blocks (segments/find-comment-blocks segments))
  (when (empty? comment-blocks)
    (when (dyn :debug)
      (eprint "no comment blocks found"))
    (break true))
  (when (dyn :debug)
    (eprint "first comment block found was: " (first comment-blocks)))
  # output rewritten content
  (let [rewritten (try
                    (rewrite/rewrite-with-verify comment-blocks)
                    ([err]
                      (def {:ev-form ev-form
                            :line line
                            :offset offset} err)
                      (eprint)
                      (eprintf "Mal-formed value: `%s` in: `%s` line: %d"
                               ev-form
                               input
                               (dec (+ line offset)))
                      nil))]
    (when (nil? rewritten)
      (break false))
    (buffer/blit buf rewritten -1))
  (if (not= "" output)
    (spit output buf)
    (print buf))
  true)

# since there are no tests in this comment block, nothing will execute
(comment

  (def file-path "./generate.janet")

  # output to stdout
  (generate/handle-one {:input file-path
                        :output ""})

  # output to file
  (generate/handle-one {:input file-path
                        :output (string "/tmp/"
                                        name/prog-name
                                        "-test-output.txt")})

  )
(defn utils/rand-string
  [n]
  (->> (os/cryptorand n)
       (map |(string/format "%02x" $))
       (string/join)))

(comment

  (let [len 8
        res (utils/rand-string len)]
    (truthy? (and (= (length res) (* 2 len))
                  # only uses hex
                  (all |(peg/find '(range "09" "af" "AF") # :h
                                  (string/from-bytes $))
                       res))))
  # => true

  )

(defn utils/no-ext
  [file-path]
  (when file-path
    (if-let [rev (string/reverse file-path)
             dot (string/find "." rev)]
      (string/reverse (string/slice rev (inc dot)))
      file-path)))

(comment

  (utils/no-ext "fun.janet")
  # => "fun"

  (utils/no-ext "/etc/man_db.conf")
  # => "/etc/man_db"

  )

(defn judges/make-judges
  [src-root judge-root]
  (def subdirs @[])
  (def out-in-tbl @{})
  (defn helper
    [src-root subdirs judge-root]
    (each path (os/dir src-root)
      (def in-path (path/join src-root path))
      (case (os/stat in-path :mode)
        :directory
        (do
          (helper in-path (array/push subdirs path)
                  judge-root)
          (array/pop subdirs))
        #
        :file
        (when (string/has-suffix? ".janet" in-path)
          (def judge-file-name
            (string (utils/no-ext path) ".judge"))
          (let [out-path (path/join judge-root
                                    ;subdirs
                                    judge-file-name)]
            (unless (generate/handle-one {:input in-path
                                          :output out-path})
              (eprintf "Test generation failed for: %s" in-path)
              (eprintf "Please confirm validity of source file: %s" in-path)
              (error nil))
            (put out-in-tbl
                 (path/abspath out-path)
                 (path/abspath in-path)))))))
  #
  (helper src-root subdirs judge-root)
  out-in-tbl)

# since there are no tests in this comment block, nothing will execute
(comment

  (def proj-root
    (path/join (os/getenv "HOME")
               "src" name/prog-name))

  (def judge-root
    (path/join proj-root name/dot-dir-name))

  (def src-root
    (path/join proj-root name/prog-name))

  (os/mkdir judge-root)

  (judges/make-judges src-root judge-root)

  )

(defn judges/find-judge-files
  [dir]
  (def file-paths @[])
  (defn helper
    [dir file-paths]
    (each path (os/dir dir)
      (def full-path (path/join dir path))
      (case (os/stat full-path :mode)
        :directory
        (helper full-path file-paths)
        #
        :file
        (when (string/has-suffix? ".judge" path)
          (array/push file-paths [full-path path]))))
    file-paths)
  #
  (helper dir file-paths))

(defn judges/execute-command
  [opts]
  (def {:command command
        :count count
        :judge-file-rel-path jf-rel-path
        :results-dir results-dir
        :results-full-path results-full-path} opts)
  (when (dyn :debug)
    (eprintf "command: %p" command))
  (let [jf-rel-no-ext (utils/no-ext jf-rel-path)
        err-path
        (path/join results-dir
                   (string "stderr-" count "-" jf-rel-no-ext ".txt"))
        out-path
        (path/join results-dir
                   (string "stdout-" count "-" jf-rel-no-ext ".txt"))]
    (try
      (with [ef (file/open err-path :w)]
        (with [of (file/open out-path :w)]
          (let [ecode (os/execute command :px {:err ef
                                               :out of})]
            (when (not (zero? ecode))
              (eprintf "non-zero exit code: %d" ecode)))
          (file/flush ef)
          (file/flush of)))
      ([_]
        (error {:out-path out-path
                :err-path err-path
                :type :command-failed}))))
  (def marshalled-results
    (try
      (slurp results-full-path)
      ([err]
        (eprintf "Failed to read in marshalled results from: %s"
                 results-full-path)
        (error nil))))
  # resurrect the results
  (try
    (unmarshal (buffer marshalled-results))
    ([err]
      (eprintf "Failed to unmarshal content from: %s"
               results-full-path)
      (error nil))))

(defn judges/make-results-dir-path
  [judge-root]
  (path/join judge-root
             (string "." (os/time) "-"
                     (utils/rand-string 8) "-"
                     "judge-gen")))

(comment

  (peg/match ~(sequence (choice "/" "\\")
                        "."
                        (some :d)
                        "-"
                        (some :h)
                        "-"
                        "judge-gen")
             (judges/make-results-dir-path ""))
  # => @[]

  )

(defn judges/ensure-results-full-path
  [results-dir fname i]
  (let [fpath (path/join results-dir
                         (string i "-" (utils/no-ext fname) ".jimage"))]
    # note: create-dirs expects a path ending in a filename
    (jpm/create-dirs fpath)
    (unless (os/stat results-dir)
      (eprintf "Failed to create dir for path: %s" fpath)
      (error nil))
    fpath))

(defn judges/judge-all
  [judge-root test-src-tbl]
  (def results @{})
  (def file-paths
    (sort (judges/find-judge-files judge-root)))
  (var count 0)
  (def results-dir (judges/make-results-dir-path judge-root))
  #
  (each [jf-full-path jf-rel-path] file-paths
    (print "  " jf-rel-path)
    (def results-full-path
      (judges/ensure-results-full-path results-dir jf-rel-path count))
    (when (dyn :debug)
      (eprintf "results path: %s" results-full-path))
    # backticks below for cross platform compatibility
    (def command [(dyn :executable "janet")
                  "-e" (string "(os/cd `" judge-root "`)")
                  "-e" (string "(do "
                               "  (setdyn :judge-gen/test-out "
                               "          `" results-full-path "`) "
                               "  (dofile `" jf-full-path "`) "
                               ")")])
    (when (dyn :debug)
      (eprintf "command: %p" command))
    (def results-for-path
      (try
        (judges/execute-command
          {:command command
           :count count
           :judge-file-rel-path jf-rel-path
           :results-dir results-dir
           :results-full-path results-full-path})
        ([err]
          (when err
            (if-let [err-type (err :type)]
              # XXX: if more errors need to be handled, check err-type
              (let [{:out-path out-path
                     :err-path err-path} err]
                (eprint)
                (eprintf "Command failed:\n  %p" command)
                (eprint)
                (eprint "Potentially relevant paths:")
                (eprintf "  %s" jf-full-path)
                #
                (def err-file-size (os/stat err-path :size))
                (when (pos? err-file-size)
                  (eprintf "  %s" err-path))
                #
                (eprint)
                (when (pos? err-file-size)
                  (eprint "Start of test stderr output")
                  (eprint)
                  (eprint (string (slurp err-path)))
                  #(eprint)
                  (eprint "End of test stderr output")
                  (eprint)))
              (eprintf "Unknown error:\n %p" err)))
          (error nil))))
    (def src-full-path
      (in test-src-tbl jf-full-path))
    (assert src-full-path
            (string "Failed to determine source for test: " jf-full-path))
    (put results
         src-full-path results-for-path)
    (++ count))
  results)

(defn summary/report
  [results]
  (when (empty? results)
    (eprint "No test results")
    (break true))
  (var total-tests 0)
  (var total-passed 0)
  (def failures @{})
  # analyze results
  (eachp [fpath test-results] results
    (def name (path/basename fpath))
    (when test-results
      (var passed 0)
      (var num-tests (length test-results))
      (var fails @[])
      (each test-result test-results
        (++ total-tests)
        (def {:passed test-passed} test-result)
        (if test-passed
          (do
            (++ passed)
            (++ total-passed))
          (array/push fails test-result)))
      (when (not (empty? fails))
        (put failures fpath fails))))
  # report any failures
  (var i 0)
  (each fpath (sort (keys failures))
    (def failed-tests (get failures fpath))
    (each fail failed-tests
      (def {:test-value test-value
            :expected-value expected-value
            :name test-name
            :passed test-passed
            :test-form test-form} fail)
      (++ i)
      (print)
      (prin "--(")
      (display/print-color i :cyan)
      (print ")--")
      (print)
      (display/print-color "source file:" :yellow)
      (print)
      (display/print-color (string (utils/no-ext fpath) ".janet") :red)
      (print)
      (print)
      #
      (display/print-color "failed:" :yellow)
      (print)
      (display/print-color test-name :red)
      (print)
      #
      (print)
      (display/print-color "form" :yellow)
      (display/print-form test-form)
      #
      (print)
      (display/print-color "expected" :yellow)
      (display/print-form expected-value)
      #
      (print)
      (display/print-color "actual" :yellow)
      (display/print-form test-value :blue)))
  (when (zero? (length failures))
    (print)
    (print "No tests failed."))
  # summarize totals
  (print)
  (display/print-dashes)
  (when (= 0 total-tests)
    (print "No tests found, so no judgements made.")
    (break true))
  (if (not= total-passed total-tests)
    (display/print-color total-passed :red)
    (display/print-color total-passed :green))
  (prin " of ")
  (display/print-color total-tests :green)
  (print " passed")
  (display/print-dashes)
  (= total-passed total-tests))

(comment

  (summary/report @{})
  # => true

  (def results
    '@[{:expected-value true
        :passed true
        :name "line-6"
        :test-form (validate/valid-code? "true")
        :type :is
        :expected-form true
        :test-value true}
       {:expected-value false
        :passed true
        :name "line-9"
        :test-form (validate/valid-code? "(")
        :type :is
        :expected-form false
        :test-value false}
       {:expected-value true
        :passed true
        :name "line-12"
        :test-form (validate/valid-code? "()")
        :type :is
        :expected-form true
        :test-value true}
       {:expected-value false
        :passed true
        :name "line-15"
        :test-form (validate/valid-code? "(]")
        :type :is
        :expected-form false
        :test-value false}])

  (let [buf @""]
    (with-dyns [:out buf]
      (summary/report @{"validate.jimage" results}))
    (string/has-prefix? "\nNo tests failed." buf))
  # => true

  )

(defn runner/handle-one
  [opts]
  (def {:judge-dir-name judge-dir-name
        :proj-root proj-root
        :src-root src-root} opts)
  (def judge-root
    (path/join proj-root judge-dir-name))
  (try
    (do
      (display/print-dashes)
      (print)
      (print (string name/prog-name " is starting..."))
      (print)
      (display/print-dashes)
      # remove old judge directory
      (prin "Cleaning out: " judge-root " ... ")
      (jpm/rm judge-root)
      # make a fresh judge directory
      (os/mkdir judge-root)
      (print "done")
      # copy source files
      (prin "Copying source files... ")
      # shhhhh
      (with-dyns [:out @""]
        # each item copied separately for platform consistency
        (each item (os/dir src-root)
          (def full-path (path/join src-root item))
          (jpm/copy-continue full-path judge-root)))
      (print "done")
      # create judge files
      (prin "Creating tests files... ")
      (flush)
      (def ts-tbl
        (judges/make-judges src-root judge-root))
      (print "done")
      # judge
      (print "Running tests...")
      (def results
        (judges/judge-all judge-root ts-tbl))
      (display/print-dashes)
      # summarize results
      (def all-passed
        (summary/report results))
      (print)
      # XXX: if detecting that being run via `jpm test` is possible,
      #      may be can show following only when run from `jpm test`
      (print (string name/prog-name
                     " is done, later output may be from `jpm test`"))
      (print)
      (display/print-dashes)
      all-passed)
    #
    ([err]
      (when err
        (eprint "Unexpected error:\n")
        (eprintf "\n%p" err))
      (eprint "Runner stopped")
      nil)))

# since there are no tests in this comment block, nothing will execute
(comment

  (def proj-root
    (path/join (os/getenv "HOME")
               "src" name/prog-name))

  (def src-root
    (path/join proj-root name/prog-name))

  (runner/handle-one {:judge-dir-name name/dot-dir-name
                      :proj-root proj-root
                      :src-root src-root})

  )
(def sample/content
  ``
  # This comment block has tests
  (comment

    # 1. A comment block test has two pieces:
    #
    # a. The form
    # b. The expected value comment
    #
    (+ 1 1)
    # => 2

    # 2. Another example

    # The following form is executed, as it has no expected value, but its
    # effects will remain valid for subsequent portions
    (def my-value 1)

    # Note the use of `my-value` here
    (struct :a my-value :b 2)
    # => {:a 1 :b 2}

    # 3. Below shows one way to express nested content more readably
    (let [i 2]
      (deep=
        #
        (struct :a (/ i i)
                :b [i (+ i 1) (+ i 3) (+ i 5)]
                :c {:x (math/pow 2 3)})
        #
        {:a 1
         :b [2 3 5 7]
         :c {:x 8}}))
    # => true

    # 4. One way to express expected values involving tuples, is to use
    #    square bracket tuples...
    (tuple :a :b)
    # => [:a :b]

    # 5. Alternatively, quote ordinary tuples in the expected value comment
    (tuple :x :y :z)
    # => '(:x :y :z)

    )

  # This comment block does not have tests because there are
  # no expected values expressed as line comments
  (comment

    (print "hi")

    (each x (range 3)
      (print x))

    )

  # In expected value comments, only expressions that yield values that
  # can be used with `marshal` will work.  (The reason for this
  # limitation is because `marshal` / `unmarshal` are used to save and
  # restore test results which are aggregated to produce an overall
  # summary.)
  (comment

    # Thus the following will not work:

    # (comment
    #
    #   printf
    #   # => printf
    #
    #   )

    # as `marshal` cannot handle `printf`:

    # repl:1:> (marshal printf)
    # error: no registry value and cannot marshal <cfunction printf>
    #   in marshal
    #   in _thunk [repl] (tailcall) on line 2, column 1

    # Though not equivalent, one can do this sort of thing instead:

    (describe printf)
    # => "<cfunction printf>"

    # or may be this is sufficient in some cases:

    (type printf)
    # => :cfunction

    )

  ``)

# from the perspective of `jpm test`
(def proj-root
  (path/abspath "."))

# XXX: hack to prevent from running when testing
(when (nil? (dyn :judge-gen/test-out))
  (let [src-root "usages"
        judge-dir-name ".janet-usages"]
    # ensure src-root exists
    (var was-no-src-root false)
    (unless (os/stat src-root)
      (set was-no-src-root true)
      (eprintf "creating %s directory..." src-root)
      (os/mkdir src-root))
    (def stat (os/stat src-root))
    (unless (and stat
                 (= :directory (stat :mode)))
      (eprint "src-root must be a directory: " src-root)
      (os/exit 1))
    # create sample file if there was no src-root
    (when was-no-src-root
      (def sample-file-path
        (path/join src-root "sample.janet"))
      (unless (os/stat sample-file-path)
        (eprintf "creating sample file: %s" sample-file-path)
        (spit sample-file-path sample/content)))
    # generate and run tests
    (let [all-passed (runner/handle-one
                       {:judge-dir-name judge-dir-name
                        :proj-root proj-root
                        :src-root src-root})]
      (when (not all-passed)
        (os/exit 1)))))
