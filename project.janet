(declare-project
  :name "th"
  :author "saikyun"
  :dependencies [## using my own fork due to problem with latest raylib release
                 "https://github.com/Saikyun/jaylib"
                 ###"file:////Users/test/programmering/janet/my-jaylib"
                 "https://github.com/janet-lang/spork"])

(declare-native
  :name "text-rendering"
  :source @["src/text_rendering.c"])

(def proj-root
  (os/cwd))

(def src-root
  (string proj-root "/src"))

(declare-source
  :source [(string src-root "/main.janet")])

(comment
  (declare-executable
    :name "myexec"
    :entry (string src-root "/main.janet")))

(phony "judge" ["build"]
       (os/execute ["jg-verdict"
                    "-p" proj-root
                    "-s" src-root] :p))
