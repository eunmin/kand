(ns kand.core
  (:require [kand.type :refer :all]))

(defn plus [x y]
  (+ (:val x) (:val y)))

(def core-env {"+" (->Primitive plus)})
