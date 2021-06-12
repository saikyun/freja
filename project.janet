(declare-project
  :name "Freja"
  :author "saikyun"
  :dependencies ["https://github.com/janet-lang/spork"

                 ## using my own fork due to additions to jaylib
                 "https://github.com/Saikyun/freja-jaylib"

                 # example of how to use `:tag`
                 # {:repo "https://...." :tag "abcdcbdc"}
])

(declare-native
  :name "text-rendering"
  :source @["src/text_rendering.c"])

(def proj-root
  (os/cwd))

(def src-root
  (string proj-root "/src"))

(declare-source
  :source @[src-root "freja"])

(declare-executable
  :name "freja"
  :entry (string src-root "/main.janet")
  :install true)

(phony "judge" ["build"]
       (os/execute ["jg-verdict"
                    "-p" proj-root
                    "-s" src-root] :p))
