(ns prisoners.game
  (:require [clojure.string :as st :only join]
            [prisoners.strategy :as stg])
  (:use [clojure.math.numeric-tower :as math :only (round expt)]))

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
      (update-in [:points] conj x)
      (update-in [:opponent] conj (inferred-play x))))

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
