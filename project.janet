(declare-project
  :name "Freja"
  :author "saikyun"
  :dependencies [## using my own fork due to problem with latest raylib release
                 "https://github.com/Saikyun/freja-jaylib"
                 #"file:////Users/test/programmering/janet/freja-jaylib"
                 
                 # example of how to use `:tag`
                 # {:repo "https://...." :tag "abcdcbdc"}
                 
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
