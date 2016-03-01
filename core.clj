(ns clojure-test-generated-graphs.core
)


(require '[clojure.string :as string])


(defn datasource [name t & rest]
  (println (string/join ["Called Datasource: " name]))
  (Thread/sleep t)
)

(defn somethingelse [t & rest]
  (println "Did something else.")
  (Thread/sleep t)
)

(defn run-test []
  (let [six (datasource "foo" 1000 []) seven (somethingelse 1000 []) eight (datasource "foo" 1000 []) nine (somethingelse 1000 [])]
    (let [three (somethingelse 1000 seven nine) four (datasource "foo" 1000 seven eight) five (datasource "foo" 1000 nine)] 
      (let [one (datasource "foo" 1000 three four nine) two (somethingelse 1000 six five)]
        (somethingelse 1 one two)
        )
      )
    ) 
  )


(defn -main
  "Run and time the application"
  [& args]
  (time (run-test))
)

