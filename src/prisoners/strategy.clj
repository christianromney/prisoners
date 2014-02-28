(ns prisoners.strategy
  (:require [prisoners.utils :as utils]))

(defprotocol Scorable
  (score [strategy])
  (total [strategy]))

(defrecord Strategy [name points plays opponent]
  Scorable
  (score [strategy]
    (->> strategy :points (reductions +)))
  (total [strategy]
    (reduce + (:points strategy))))

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
  (utils/add-to this :plays :coop))

;; The `:random` strategy is a baseline for comparison.
;; Any given move is randomly chosen to be cooperate or
;; defect.
(defmethod play :random [this]
  (utils/add-to this :plays (rand-nth [:defect :coop])))
