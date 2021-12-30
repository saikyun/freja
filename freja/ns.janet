(import spork/path)

(defn- remove-ext
  [path]
  (let [ext (path/ext path)
        name (string/slice path 0 (- (inc (length ext))))]
    name))

(defn start
  [&opt name]
  (default name (remove-ext (dyn :current-file)))

  (assert (string? name) (string/format "%P is not a string" name))

  (def existing (if (string? name)
                  (-> (module/find name)
                      first
                      module/cache)
                  (module/cache name)))
  
  (setdyn :ns/name name)

  #(print "ns name: " name)
  
  (setdyn :ns/old-env
          (when existing
            #(print "old exists")
            existing))

  (when existing
    #(table/setproto (curenv) existing)
    (merge-into (curenv) existing)
    )
  )

(defn stop
  []
  (def env (curenv))
  (def old-env (dyn :ns/old-env))
  
  (loop [[k v] :pairs env
         :when (table? v)
         :let [{:value value
                :ref ref
                :macro macro} v
               {:ref old-ref
                :ns/nsed old-nsed} (get old-env k {})]
         :when value
         :when (not macro)]
    (cond
      old-ref
      (do
        #(print "replacing ref " k)
        (put v :ref old-ref))

      (not ref)
      (put v :ref @[]))
    
    (put-in v [:ref 0] value)
    (put v :ns/nsed (inc (or old-nsed 0)))
    (put v :value nil))

  (pp (dyn :ns/name))
  #(put module/cache (dyn :ns/name) (curenv))

  #(put nses (dyn :ns/name) (curenv))
  )
 