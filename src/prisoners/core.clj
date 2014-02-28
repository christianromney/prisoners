(ns prisoners.core
  (:require [clojure.string :as st :only join]
            [prisoners.strategy :as stg]
            [prisoners.utils :as utils])
  (:use [incanter core charts])
  (:use [clojure.math.numeric-tower :as math :only (round expt)]))
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

;; ### Payoffs

;; The payoff amounts are held in dynamic vars that can be rebound
;; to different values if desired. The actual values are not important so long as
;; mutual cooperation is more rewarding than mutual defection,
;; mutual defection is more rewarding than cooperation when the other
;; defects and defection when the other cooperates is more
;; rewarding than all other combinations.

(def ^{:dynamic true} *sucker*
  "Payoff for the cooperator when the other strategy defects."
  0)

(def ^{:dynamic true} *defector*
  "Payoff when both strategies defect."
  1)

(def ^{:dynamic true} *partner*
 "Payoff when both strategies cooperate."
  3)

(def ^{:dynamic true} *backstabber*
  "Payoff for the defector when the other cooperates."
  5)

;; ### Predicates
(defn mutual-cooperation?
  "Mutual cooperation is when both strategies cooperated."
  [& players]
  (every? #{:coop} players))

(defn mutual-defection?
  "Mutual defection is when both strategies defected."
  [& players]
  (every? #{:defect} players))

(defn betrayal?
  "Betrayal is when the first strategy defected and the second cooperated."
  [x y]
  (and (= :defect x) (= :coop y)))

(defn inferred-play
  "Infers what the opponent played based on payoff.
  If my payoff is less than `*partner*` my opponent must have betrayed me."
  [payoff]
  (if (< payoff *partner*) :defect :coop))

;; ### Payoff Functions
(defn pay
  "The mechanics of payment includes both adding the points for the round and
  recording the opponent's last play."
  [strategy x]
  (-> strategy
      (utils/add-to :points x)
      (utils/add-to :opponent (inferred-play x))))

(defn pay-partners
  "Pay both strategies for cooperation."
  [a b]
  (map #(pay % *partner*) [a b]))

(defn pay-defectors
  "Pay both strategies for defection."
  [a b]
  (map #(pay % *defector*) [a b]))

(defn pay-betrayal
  "Reward the backstabber and punish the sucker."
  [backstabber sucker]
  [(pay backstabber *backstabber*)
   (pay sucker *sucker*)])

;; ### Gameplay Functions

(defn play-round
  "Play one round between two strategies and award the appropriate payoffs."
  [[x y]]
  (let [player-a (stg/play x)
        player-b (stg/play y)
        a (last (:plays player-a))
        b (last (:plays player-b))]
    (cond (mutual-cooperation? a b) (pay-partners player-a player-b)
          (mutual-defection? a b) (pay-defectors player-a player-b)
          (betrayal? a b) (pay-betrayal player-a player-b)
          :else (pay-betrayal player-b player-a))))

(defn play-rounds
  "Plays a given number of rounds between two named strategies.
  Example: `(play-rounds 10 :sucker :cheat)`"
  [rounds x y]
  (nth (iterate play-round (map stg/strategy-constructor [x y])) rounds))

(defn winner?
  "outputs the winning strategy"
  [a b]
  (cond (> (stg/total a) (stg/total b))
          (str (:name a) " wins!")
        (= (stg/total a) (stg/total b))
          (str "tie!")
        :else
          (str (:name b) "wins!")))

(defn chart
  "Creates a line chart of the points accumulated by two opponents"
  [strategies]
  (let [[a b] strategies
        title (str (:name a) " vs " (:name b) " (" (winner? a b) ")")
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

(comment
  (graph (play-rounds 30 :random :sucker))
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
