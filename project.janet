# freja-jaylib-shim doesn't need opengl etc
# which makes it easier to run in CI
(def freja-jaylib-dep
  (if (= "true" (os/getenv "FREJA_TEST"))
    "https://github.com/saikyun/freja-jaylib-shim"
    "https://github.com/saikyun/freja-jaylib"))

(declare-project
  :name "freja"
  :author "Jona Ekenberg <saikyun@gmail.com>"
  :license "MIT"
  :description "Extendable text editor with a focus on quick game development and GUI creation. Like a minimal emacs with easy opengl access."
  :url "https://github.com/saikyun/freja"
  :repo "git+https://github.com/saikyun/freja"
  :dependencies ["https://github.com/janet-lang/spork"

                 {:repo "https://github.com/saikyun/janet-bounded-queue" :tag "main"}

                 {:repo "https://github.com/saikyun/janet-whereami" :tag "main"}

                 {:repo "https://github.com/saikyun/freja-layout" :tag "main"}

                 {:repo "https://github.com/Saikyun/janet-profiling" :tag "main"}

                 ## using my own fork due to additions to jaylib
                 {:repo freja-jaylib-dep :tag "main"}

                 # example of how to use `:tag`
                 # {:repo "https://...." :tag "abcdcbdc"}
])


(def lflags
  (case (os/which)
    :windows '[]
    :macos '["-Wl,-export_dynamic"] # need to test this
    :linux '["-rdynamic"] # I want this for more OSes, needed to load native modules from freja binary
    #default
    '["-lpthread"]))

(def proj-root
  (os/cwd))

(def sep (if (= (os/which) :windows) "\\" "/"))

(def src-root
  (string proj-root sep "freja"))

(declare-source
  :source @["freja"])

(declare-executable
  :name "freja"
  :entry (string src-root sep "main.janet")
  :lflags lflags
  :install true)

(phony "judge" ["build"]
       (os/execute ["jg-verdict"
                    "-p" proj-root
                    "-s" src-root] :p))
