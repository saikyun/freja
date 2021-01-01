(declare-project
 :name "th"
 :author "saikyun"
 :dependencies [## using my own fork due to problem with latest raylib release
                "https://github.com/Saikyun/jaylib"
                "https://github.com/janet-lang/spork"])

(declare-executable
 :name "myexec"
 :entry "main.janet")

