(ns prisoners.core
  (:require clojure.string)
  (:use [clojure.contrib.combinatorics :as combo]))

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

;; The payoff amounts are held in private vars that are visible
;; only within the namespace.

;; Payoff for the cooperator when the other strategy defects.
(def ^{:private true} *sucker* 0)

;; Payoff when both strategies defect.
(def ^{:private true} *defector* 1)

;; Payoff when both strategies cooperate.
(def ^{:private true} *partner* 3)

;; Payoff for the defector when the other cooperates.
(def ^{:private true} *backstabber* 5)


;; ### Predicates

;; Mutual cooperation is when both strategies cooperated.
(defn mutual-cooperation? [& players]
  (every? #{:coop} players))

;; Mutual defection is when both strategies defected.
(defn mutual-defection? [& players]
  (every? #{:defect} players))

;; Betrayal is when the first strategy defected and the 
;; second cooperated.
(defn betrayal? [x y]
  (and (= :defect x) (= :coop y)))

;; ### Helper Functions

;; General-purpose map collection modifier.
(defn add-to [strategy k v]
  (update-in strategy [k] conj v))

;; Infers what the opponent played based on payoff.
;; If my payoff is less than `*partner*` my opponent
;; must have betrayed me.
(defn inferred-play [payoff]
  (if (< payoff *partner*) :defect :coop))

;; ### Payoff Functions

;; The mechanics of payment includes both
;; adding the points for the round and 
;; recording the opponent's last play.
(defn pay [strategy x]
  (-> strategy
      (add-to :points x)
      (add-to :opponent (inferred-play x))))

;; Pay both strategies for cooperation.
(defn pay-partners [a b]
  (map #(pay % *partner*) [a b]))

;; Pay both strategies for defection.
(defn pay-defectors [a b]
  (map #(pay % *defector*) [a b]))

;; Reward the backstabber and punish the sucker.
(defn pay-betrayal [backstabber sucker]
  [(pay backstabber *backstabber*)
   (pay sucker *sucker*)])

;; ### Strategy Implementations

;; The `play` multimethod dispatches on the
;; data structure's `:name` attribute.
(defmulti play :name)

;; The `:sucker` strategy always cooperates
(defmethod play :sucker [this]
  (add-to this :plays :coop))

;; The `:cheat` strategy always defects
(defmethod play :cheat [this]
  (add-to this :plays :defect))
  
;; The `:grudger` strategy will cooperate until its
;; opponent defects. Thereafter, it will always defect.
;; It is less *forgiving* than `:tit-for-tat`.
(defmethod play :grudger [this]
  (add-to this :plays
    (if (some #{:defect} (:opponent this)) :defect :coop)))

;; The `:tit-for-tat` strategy plays whatever
;; its opponent played last and cooperates
;; if given the first move.
(defmethod play :tit-for-tat [this]
  (add-to this :plays (or (last (:opponent this)) :coop)))

;; The `:random` strategy is a baseline for comparison.
;; Any given move is randomly chosen to be cooperate or
;; defect.
(defmethod play :random [this]
  (add-to this :plays (rand-nth [:defect :coop])))

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
          (betrayal? a b) (pay-betrayal player-a player-b)
          :else (pay-betrayal player-b player-a))))

;; Initializes a data structure for the named strategy
(defn strategy-map [named]
  {:name named, :points [], :plays [], :opponent []})

;; Plays a given number of rounds between two named strategies.
;; Example: `(play-rounds 10 :sucker :cheat)`
(defn play-rounds [rounds x y]
  (nth (iterate play-round (map strategy-map [x y]))
       (inc rounds)))

;; Produce a total score as the sum of the points awarded during each round.
(defn total [strategy]
  (reduce + (:points strategy)))

;; Summarizes a strategy's score.
(defn summarize [strategy]
  (str (:name strategy) ": " (total strategy) " points"))

;; Calculates the resulting scores for each of the strategies.
(defn report [strategies]
  (clojure.string/join ", " (map summarize strategies)))

;; ### Running the Simulation
;;
;; First, run `lein deps` to download the dependencies
;;
;; Then, run `lein repl` and type this at the prompt:
;;
;; <pre><code>
;;  (ns game (:use prisoners.core)) 
;;
;;  (report (play-rounds 10 :sucker :cheat))
;;
;; </code></pre>
