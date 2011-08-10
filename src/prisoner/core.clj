(ns prisoner.core)

(defprotocol Prisoner 
  (play [this])
  (pay [this x])
  (total [this]))

(defrecord Defector [points plays opponent]
  Prisoner
  (total [_] (reduce + 0 points))
  (play [_] (Defector. points (conj plays :defect) opponent))
  (pay [_ x] (Defector. (conj points x) plays
                           (if (< x 3) (conj opponent :defect)
                             (conj opponent :coop)))))

(defrecord TitForTat [points plays opponent]
  Prisoner
  (total [_] (reduce + 0 points))
  (play [_] (TitForTat. points
                           (let [opp (last opponent)] 
                             (if (= :defect opp) (conj plays :defect)
                               (conj plays :coop)))
                           opponent))
  (pay [_ x] (TitForTat. (conj points x) plays
                            (if (< x 3) (conj opponent :defect)
                              (conj opponent :coop)))))

