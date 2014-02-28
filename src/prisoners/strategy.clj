(ns prisoners.strategy
  (:require [prisoners.utils :as utils]))

(defprotocol Playable
  "A playable abstraction has an internal play function
   that implements the mechanics of making a move"
  (-play [playable move]))


(defprotocol Scorable
  "A scorable abstraction reports its scores and total"
  (scores [strategy])

  (total [strategy]))

(defrecord Strategy [name points plays opponent]
  Scorable

  (scores [this]
    (->> this :points (reductions +)))

  (total [this]
    (reduce + (:points this)))

  Playable

  (-play [this move]
    (update-in this [:plays] conj move)))

(defn strategy-constructor
  "Initializes a data structure for the named strategy."
  [named]
  (->Strategy named [] [] []))

;; ### Strategy Implementations

(defmulti play
  "The `play` multimethod dispatches on the data structure's `:name` attribute."
  :name)

;; The `:sucker` strategy always cooperates
(defmethod play :sucker [this]
  (-play this :coop))

;; The `:random` strategy is a baseline for comparison.
;; Any given move is randomly chosen to be cooperate or
;; defect.
(defmethod play :random [this]
  (-play this (rand-nth [:defect :coop])))
