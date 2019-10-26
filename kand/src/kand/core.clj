(ns kand.core)

(defn plus [x y]
  (+ (:val x) (:val y)))

(def core-env {"+" plus})
