(import freja/open-file)

(defn jump-to-def
  [symbol &keys {:env env}]
  (def old-env (curenv))
  (when env
    (fiber/setenv (fiber/current) env))
  (let [sm (get (dyn symbol) :source-map)]
    (tracev sm)
    (open-file/open-file
      (first sm)
      ;(map dec (drop 1 sm))))
  (fiber/setenv (fiber/current) old-env))

(comment
  (jump-to-def 'jump-to-def)
  (dyn 'jump-to-def)
  )