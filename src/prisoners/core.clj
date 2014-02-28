(ns prisoners.core
  (:require [clojure.string :as st :only join]
            [prisoners.game :as game]
            [prisoners.strategy :as stg])
  (:use [incanter core charts])
  (:use [clojure.math.numeric-tower :as math :only (round expt)])
  (:gen-class))


;; ### Game visualizations

(defn chart
  "Creates a line chart of the points accumulated by two opponents"
  [strategies]
  (let [[a b] strategies
        title (str (:name a) " vs " (:name b) " (" (game/winner? a b) ")")
        rounds (range 1 (-> a :plays count inc))]
    (-> (line-chart rounds (stg/scores a) :series-label (:name a) :legend true)
        (add-categories rounds (stg/scores b) :series-label (:name b) :legend true)
        (set-x-label "Round")
        (set-y-label "Points")
        (set-title title)
        (set-theme :default))))

(defn graph
  "Displays a chart showing multiple rounds of play
  between two strategies."
  [results]
  (-> results chart view))

;; ### Entry point

(defn -main
  "Application entry point"
  [& args]
  (println args)
  (let [rounds (Integer/parseInt (first args))
        strategies (map keyword (rest args))]
    (graph (apply (partial game/play-rounds rounds) strategies))))
