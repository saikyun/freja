(declare-project
  :name "th"
  :author "saikyun"
  :dependencies [## need to build jaylib myself, if regular jaylib works for you, you can use the dep
                 ##"https://github.com/janet-lang/jaylib"
                 "https://github.com/janet-lang/spork"])

(declare-executable
  :name "myexec"
  :entry "main.janet")
