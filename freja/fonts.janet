(use jaylib)
(import spork/path)
(import ./dumb :prefix "")
(import ./text_rendering :as tr)

(def default-font-conf @{})

(varfn glyphs->size-struct
  [conf glyphs]
  (table ;(interleave
            glyphs
            (tr/measure-each-char conf glyphs))))

(def default-glyphs (string/bytes " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~\\\r"))

(defn default-load-font
  [font-path size]
  (def [x-scale _] screen-scale)
  (load-font-ex font-path (* x-scale size) default-glyphs))

(defn default-load-font-from-memory
  [kind font-data size]
  (def [x-scale _] screen-scale)
  (load-font-from-memory kind
                         font-data
                         (length font-data)
                         (* x-scale size)
                         default-glyphs))

(defn load-font
  [text-data opts]
  (let [font (load-font-ex (opts :font-path) (opts :size) (opts :glyphs))]

    (put opts :font font)

    (def t (freeze opts))

    (put text-data :conf t)
    (put text-data :sizes (glyphs->size-struct t (t :glyphs)))

    t))

(defn load-font-from-mem
  [text-data opts]

  (let [font (load-font-from-memory (opts :ext)
                                    (opts :font-data)
                                    (length (opts :font-data))
                                    (opts :size)
                                    (opts :glyphs))]

    (put opts :font font)

    (def t (freeze opts))

    (put text-data :conf t)
    (put text-data :sizes (glyphs->size-struct t (t :glyphs)))

    t))

# these are slurped on top level in order
# to be included in the binary when
# running `jpm build`
(def mplus (slurp (path/join "fonts" "MplusCodeLatin60-Medium.otf")))
(def poppins (slurp (path/join "fonts" "Poppins-Regular.otf")))
(def ebgaramond (slurp (path/join "fonts" "EBGaramond12-Regular.otf")))

# storage for loaded fonts
(def fonts @{})

(defn init-default-font
  []
  (def [x-scale _] screen-scale)

  (def opts
    @{:font-data mplus
      :ext ".otf"
      :size (* 20 x-scale)
      :line-height 1.2
      :mult (/ 1 x-scale)
      :glyphs default-glyphs
      :spacing 0.5})

  (put opts :font (load-font-from-memory (opts :ext)
                                         (opts :font-data)
                                         (length (opts :font-data))
                                         (opts :size)
                                         (opts :glyphs)))

  (put opts :sizes (glyphs->size-struct opts (opts :glyphs)))

  (merge-into default-font-conf opts))
