(ns kand.core
  (:require [kand.type :refer :all])
  (:import [kand.type Num Str True False]
           [org.apache.commons.lang3.reflect ConstructorUtils MethodUtils]))

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

(defmulti to-java type)
(defmethod to-java Num [v] (:val v))
(defmethod to-java Str [v] (:val v))
(defmethod to-java True [_] true)
(defmethod to-java False [_] false)

(defn new-object [{cls :name} & args]
  (->Obj
   (ConstructorUtils/invokeConstructor
    (Class/forName cls)
    (into-array (map to-java args)))))

(defn invoke-method [{method-name :name} {obj :obj} & args]
  (->Obj (MethodUtils/invokeExactMethod obj method-name (into-array (map to-java args)))))

(def core-env {:core/*module* (->Symbol "user")
               :core/+ (->Primitive plus)
               :core/- (->Primitive minus)
               :core/> (->Primitive gt)
               :core/< (->Primitive lt)
               :core/>= (->Primitive ge)
               :core/<= (->Primitive le)
               :core/= (->Primitive eq)
               :core/new (->Primitive new-object)
               :core/. (->Primitive invoke-method)})
