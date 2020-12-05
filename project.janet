(declare-project
  :name "th"
  :author "saikyun"
  :dependencies ["https://github.com/janet-lang/jaylib"
                 "https://github.com/janet-lang/spork"])

(declare-executable
  :name "myexec"
  :entry "main.janet")
