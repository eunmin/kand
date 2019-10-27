(ns kand.core
  (:require [kand.type :refer :all]))

(defn plus [{x :val} {y :val}]
  (+ x y))

(defn minus [{x :val} {y :val}]
  (- x y))

(defn gt [{x :val} {y :val}]
  (> x y))

(defn lt [{x :val} {y :val}]
  (< x y))

(defn ge [{x :val} {y :val}]
  (>= x y))

(defn le [{x :val} {y :val}]
  (<= x y))

(defn eq [{x :val} {y :val}]
  (= x y))

(def core-env {"+" (->Primitive plus)
               "-" (->Primitive minus)
               ">" (->Primitive gt)
               "<" (->Primitive lt)
               ">=" (->Primitive ge)
               "<=" (->Primitive le)
               "=" (->Primitive eq)})

