(ns prisoner.core
  (:require clojure.string)
  (:use [clojure.contrib.combinatorics :as combo]))

;; ## Prisoner's Dilemma 
;;
;; This is an iterative Prisoner's Dilemma simulation.
;; I've selected the payoffs and names based on
;; Richard Dawkins' description in The Selfish Gene.
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
;; on the outcomes of each game using Incanter.

;; ### Payoffs

;; Payoff for the cooperator when the other strategy defects.
(def *sucker* 0)

;; Payoff when both strategies defect.
(def *defector* 1)

;; Payoff when both strategies cooperate.
(def *partner* 3)

;; Payoff for the defector when the other cooperates.
(def *backstabber* 5)

;; ### Strategy Definition

;; The Prisoner protocol defines the operations
;; that a Prisoner's Dilemma strategy must support.
(defprotocol Prisoner 
  (title [this])
  (play [this])
  (pay [this x]))

;; ### Predicates

;; Cooperation occurs when :coop is played by a strategy.
(defn cooperated? [x]
  (= :coop x))

;; Defection occurs when :defect is played by a strategy.
(defn defected? [x]
  (= :defect x))

;; Mutual cooperation is when both strategies cooperated.
(defn mutual-cooperation? [x y]
  (and (cooperated? x) (cooperated? y)))

;; Mutual defection is when both strategies defected.
(defn mutual-defection? [x y]
  (and (defected? x) (defected? y)))

;; Betrayal is when the first strategy defected and the 
;; second cooperated.
(defn betrayed? [x y]
  (and (defected? x) (cooperated? y)))

;; Infers what the opponent played based on payoff.
;; If my payoff is less than *partner* my opponent
;; must have defected.
(defn opponent-defected? [my-payoff]
  (< my-payoff *partner*))

;; ### Record-keeping Functions

;; Takes note of a cooperation by conjoining it to a sequence.
(defn note-cooperation [results]
  (conj results :coop))

;; Takes note of a defection by conjoining it to a sequence.
(defn note-defection [results]
  (conj results :defect))

;; Awards points by conjoining the value to a sequence.
(defn award [n points]
  (conj points n))

;; Pay both strategies for cooperation.
(defn pay-partners [a b]
  (map #(pay % *partner*) [a b]))

;; Pay both strategies for defection.
(defn pay-defectors [a b]
  (map #(pay % *defector*) [a b]))

;; Reward the backstaber and punish the sucker.
(defn pay-betrayal [backstabber sucker]
  [(pay backstabber *backstabber*)
   (pay sucker *sucker*)])

;; ### Strategy Implementations

;; The Sucker strategy always cooperates.
(defrecord Sucker [points plays opponent]
  Prisoner 
  (title [_] "Sucker")
  (play [_] (Sucker. points (note-cooperation plays) opponent))
  (pay [_ x] (Sucker. (award x points) plays 
                           (if (opponent-defected? x) (note-defection opponent)
                             (note-cooperation opponent)))))
  
;; The Cheat strategy always defects.
(defrecord Cheat [points plays opponent]
  Prisoner
  (title [_] "Cheat")
  (play [_] (Cheat. points (note-defection plays) opponent))
  (pay [_ x] (Cheat. (award x points) plays
                           (if (opponent-defected? x) (note-defection opponent)
                             (note-cooperation opponent)))))

;; Tit-For-Tat will always play whatever its opponent played last. 
;; It will cooperate if given the first move.
(defrecord TitForTat [points plays opponent]
  Prisoner
  (title [_] "TitForTat")
  (play [_] (TitForTat. points
                        (if (defected? (last opponent)) (note-defection plays)
                          (note-cooperation plays))
                        opponent))
  (pay [_ x] (TitForTat. (award x points) 
                         plays
                         (if (opponent-defected? x) (note-defection opponent)
                           (note-cooperation opponent)))))

;; ### Gameplay Functions

;; Play one round between two strategies and 
;; award the appropriate payoffs.
(defn play-round [[x y]]
  (let [player-a (play x) 
        player-b (play y) 
        a (last (:plays player-a)) 
        b (last (:plays player-b))]
    (cond (mutual-cooperation? a b) (pay-partners player-a player-b)
          (mutual-defection? a b) (pay-defectors player-a player-b)
          (betrayed? a b) (pay-betrayal player-a player-b)
          :else (pay-betrayal player-b player-a))))

;; Plays a given number of rounds between two named strategies.
;; Use of a macro here allows one to pass in the names of the strategies.
;; Example: (play-rounds 10 Sucker Cheat)
(defmacro play-rounds 
  [rounds x y]
  `(last 
    (take (inc ~rounds)
      (iterate play-round [(new ~x [] [] []) (new ~y [] [] [])]))))

;; Produces the sum of a sequence of numbers.
(defn tally [numbers]
  (reduce + 0 numbers))

;; Summarizes a strategy's score.
(defn summarize [strategy]
  (str (title strategy) ": " (tally (:points strategy)) " points"))

;; Calculates the resulting scores for each of the strategies.
(defn report [strategies]
  (clojure.string/join "; " (map summarize strategies)))

