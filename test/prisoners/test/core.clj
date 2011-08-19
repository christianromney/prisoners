(ns prisoners.test.core
  (:require [prisoners.core :as pri])
  (:use [clojure.test]))

;; Test Helpers
 
(defn score-round [rounds x y]
  (map (fn [m] [(:name m) (pri/total m)]) (pri/play-rounds rounds x y)))

(defn score [x]
  (last x))

;; Test Expected Outcomes

(deftest test-cheat-sucker
  (let [[x y] (score-round 1 :cheat :sucker)]
    (is 
      (and 
        (= 5 (score x)) 
        (= 0 (score y))) ":cheat 5, :sucker 0")))

(deftest test-cheat-cheat
  (let [[x y] (score-round 1 :cheat :cheat)]
    (is 
      (and 
        (= 1 (score x)) 
        (= 1 (score y))) ":cheat 1, :cheat 1")))

(deftest test-sucker-sucker
  (let [[x y] (score-round 1 :sucker :sucker)]
    (is 
      (and 
        (= 3 (score x)) 
        (= 3 (score y))) ":sucker 3, :sucker 3")))

(deftest test-cheat-tit-for-tat
  (let [[x y] (score-round 2 :cheat :tit-for-tat)]
    (is 
      (and 
        (= 6 (last x)) 
        (= 1 (last y))) ":cheat 6, :tit-fot-tat 1")))

(deftest test-cheat-grudger
  (let [[x y] (score-round 2 :cheat :grudger)]
    (is
      (and
        (= 6 (score x))
        (= 1 (score y))) ":cheat 6, :grudger 1")))

(deftest test-grudger-tit-for-tat
  (let [[x y] (score-round 2 :grudger :tit-for-tat)]
    (is
      (and
        (= 6 (last x))
        (= 6 (last y))) ":grudger 6, :tit-fot-tat 6")))
