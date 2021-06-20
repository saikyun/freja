(use jaylib)
(import ./dumb :prefix "")

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
