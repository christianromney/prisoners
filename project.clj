(defproject prisoners "0.0.2-SNAPSHOT"
  :description "A Prisoner's Dilemma simulation"
  :dev-dependencies [[lein-marginalia "0.6.0"]]
  :dependencies [[org.clojure/clojure "1.2.1"] 
                 [org.clojure/clojure-contrib "1.2.0"]]
  :aot [prisoners.core])
