(import freja/state)
(import freja/frp)
(import freja/events :as e)

(varfn eval-it
  [env code]
  (print "=> " (string/trim code))

  (try
    (do
      (fiber/setenv (fiber/current) state/user-env)
      (fiber/setenv (fiber/current) state/user-env)
      (put state/user-env :out state/out)
      (put state/user-env :err state/out)
      (def res (eval-string code))
      (e/push! frp/eval-results {:value res
                                 :code code
                                 :fiber (fiber/current)})
      (pp res))
    ([err fib]
      (debug/stacktrace fib err)

      (e/push! frp/eval-results {:error err
                                 :code code
                                 :fiber fib}))))