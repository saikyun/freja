(import freja/open-file)

(defn jump-to-def
  [symbol]
  (let [sm (get (dyn symbol) :source-map)]
    (open-file/open-file
      (first sm)
      ;(map dec (drop 1 sm)))))

(comment
  (jump-to-def 'jump-to-def)
  (dyn 'jump-to-def)
  )