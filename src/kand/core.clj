(ns kand.core
  (:require [kand.type :refer :all]))

(defn plus [{x :val} {y :val}]
  (->Num (+ x y)))

(defn minus [{x :val} {y :val}]
  (->Num (- x y)))

(defn gt [{x :val} {y :val}]
  (if (> x y) (->True) (->False)))

(defn lt [{x :val} {y :val}]
  (if (< x y) (->True) (->False)))

(defn ge [{x :val} {y :val}]
  (if (>= x y) (->True) (->False)))

(defn le [{x :val} {y :val}]
  (if (<= x y) (->True) (->False)))

(defn eq [{x :val} {y :val}]
  (if (= x y) (->True) (->False)))

(def core-env {"+" (->Primitive plus)
               "-" (->Primitive minus)
               ">" (->Primitive gt)
               "<" (->Primitive lt)
               ">=" (->Primitive ge)
               "<=" (->Primitive le)
               "=" (->Primitive eq)})

