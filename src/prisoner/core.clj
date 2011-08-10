(ns prisoner.core
  (:require clojure.string))

(defprotocol Prisoner 
  (title [this])
  (play [this])
  (pay [this x])
  (total [this]))

(defrecord Sucker [points plays opponent]
  Prisoner 
  (title [_] "Sucker")
  (total [_] (reduce + 0 points))
  (play [_] (Sucker. points (conj plays :coop) opponent))
  (pay [_ x] (Sucker. (conj points x) plays 
                           (if (< x 3) (conj opponent :defect)
                             (conj opponent :coop)))))
  

(defrecord Cheat [points plays opponent]
  Prisoner
  (title [_] "Cheat")
  (total [_] (reduce + 0 points))
  (play [_] (Cheat. points (conj plays :defect) opponent))
  (pay [_ x] (Cheat. (conj points x) plays
                           (if (< x 3) (conj opponent :defect)
                             (conj opponent :coop)))))

(defrecord Retaliator [points plays opponent]
  Prisoner
  (title [_] "Retaliator")
  (total [_] (reduce + 0 points))
  (play [_] (Retaliator. points
                           (let [opp (last opponent)] 
                             (if (= :defect opp) (conj plays :defect)
                               (conj plays :coop)))
                           opponent))
  (pay [_ x] (Retaliator. (conj points x) plays
                            (if (< x 3) (conj opponent :defect)
                              (conj opponent :coop)))))

(defn play-round [[x y]]
  (let [a (play x) b (play y) outcome-a (last (:plays a)) outcome-b (last (:plays b))]
    (cond (and (= :coop outcome-a) (= :coop outcome-b)) [(pay a 3) (pay b 3)]
          (and (= :defect outcome-a) (= :defect outcome-b)) [(pay a 1) (pay b 1)]
          (= :coop outcome-a) [(pay a 0) (pay b 5)]
          :else [(pay a 5) (pay b 0)])))

;; Example:
;;
;; (play-rounds 10 Sucker Cheat)
;; (play-rounds 10 Sucker Retaliator)

(defmacro play-rounds 
  "Plays a given number of rounds between the two given strategies
  Ex: (play-rounds 10 Sucker Cheat)"
  [rounds x y]
  `(last 
    (take (inc ~rounds)
      (iterate play-round [(new ~x [] [] []) (new ~y [] [] [])]))))

(defn report [players]
  (clojure.string/join "; " (for [x players] (str (title x) ": " (total x) " points"))))
