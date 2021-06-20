(import spork/path)

# these are slurped on top level in order
# to be included in the binary when
# running `jpm build`
(def mplus (slurp (path/join "fonts" "MplusCodeLatin60-Medium.otf")))
