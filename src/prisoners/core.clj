(ns prisoners.core
  (:require [clojure.string :as st :only join]
            [prisoners.game :as game]
            [prisoners.strategy :as stg]
            [prisoners.utils :as utils])
  (:use [incanter core charts])
  (:use [clojure.math.numeric-tower :as math :only (round expt)])
  (:gen-class))

;; ## Prisoner's Dilemma
;;
;; This is an iterative [Prisoner's Dilemma](http://en.wikipedia.org/wiki/Prisoner's_dilemma) simulation.
;; I've selected the payoffs and names based on
;; Richard Dawkins' description in [The Selfish Gene](http://www.amazon.com/gp/product/B000SEHIG2/ref=as_li_ss_tl?ie=UTF8&tag=xmlblog-20&linkCode=as2&camp=217145&creative=399373&creativeASIN=B000SEHIG2).
;;
;; Currently, a given number of rounds can be played between
;; strategies. The next goal is to organize a tournament
;; by having each strategy play every other strategy, as
;; well as a copy of itself, for a given number of rounds.
;; The most successful strategy will be determined by its
;; total number of points at the end of the tournament.
;;
;; Eventually, I wish to evolve the game (pun intended)
;; by paying each strategy not in points, but in copies of
;; itself. The most successful strategy will be determined
;; by its frequency in the pool of strategies after a given
;; number of generations.
;;
;; Both types of games above can be influenced by the number
;; of friendly or nasty strategies in the competition. The
;; final goal of this project will be to visualize and report
;; on the outcomes of each game using [Incanter](http://incanter.org/).


;; ### Game visualizations

(defn chart
  "Creates a line chart of the points accumulated by two opponents"
  [strategies]
  (let [[a b] strategies
        title (str (:name a) " vs " (:name b) " (" (game/winner? a b) ")")
        rounds (range 1 (-> a :plays count inc))]
    (-> (line-chart rounds (stg/score a) :series-label (:name a) :legend true)
        (add-categories rounds (stg/score b) :series-label (:name b) :legend true)
        (set-x-label "Round")
        (set-y-label "Points")
        (set-title title)
        (set-theme :default))))

(defn graph
  "Displays a chart showing multiple rounds of play
  between two strategies."
  [results]
  (-> results chart view))

(defn -main
  "Application entry point"
  [& args]
  (println args)
  (let [rounds (Integer/parseInt (first args))
        strategies (map keyword (rest args))]
    (graph (apply (partial game/play-rounds rounds) strategies))))

(comment
  (graph (game/play-rounds 30 :random :sucker))
  )

;; ### Running the Simulation
;;
;; First, run `lein deps` to download the dependencies
;;
;; Then, run `lein repl` and type this at the prompt:
;;
;; <pre><code>
;;  (use 'prisoners.core)
;;  (graph (play-rounds 30 :random :tit-for-tat))
;;
;; </code></pre>
