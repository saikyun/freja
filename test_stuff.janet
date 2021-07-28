(def env (make-env))

#### this works

(try (do (fiber/setenv (fiber/current) env)
       (pp (eval-string "1")))
  ([err fib]
    (debug/stacktrace fib err)))

(try (do (fiber/setenv (fiber/current) env)
       (pp (eval-string "aoe1")))
  ([err fib]
    (debug/stacktrace fib err)))

(try (do (fiber/setenv (fiber/current) env)
       (pp (eval-string "1")))
  ([err fib]
    (debug/stacktrace fib err)))



#### dofile causes env to break

(try (pp (dofile "erronous.janet" :env env))
  ([err fib]
    (debug/stacktrace fib err)))

(try (do (fiber/setenv (fiber/current) env)
       (pp (eval-string "1")))
  ([err fib]
    (debug/stacktrace fib err)))
