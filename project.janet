(declare-project
  :name "freja"
  :author "Jona Ekenberg <saikyun@gmail.com>"
  :license "MIT"
  :description "Extendable text editor with a focus on quick game development and GUI creation. Like a minimal emacs with easy opengl access."
  :url "https://github.com/saikyun/freja"
  :repo "git+https://github.com/saikyun/freja"
  :dependencies ["https://github.com/janet-lang/spork"

                 {:repo "https://github.com/saikyun/janet-whereami" :tag "main"}

                 {:repo "https://github.com/saikyun/freja-layout" :tag "main"}

                 ## using my own fork due to additions to jaylib
                 "https://github.com/saikyun/freja-jaylib"

                 # example of how to use `:tag`
                 # {:repo "https://...." :tag "abcdcbdc"}
])

(declare-native
  :name "text-rendering"
  :source @["freja/text_rendering.c"])

(def proj-root
  (os/cwd))

(def src-root
  (string proj-root "/freja"))

(declare-source
  :source @["freja"])

(declare-executable
  :name "freja"
  :entry (string src-root "/main.janet")
  :install true)

(phony "judge" ["build"]
       (os/execute ["jg-verdict"
                    "-p" proj-root
                    "-s" src-root] :p))
