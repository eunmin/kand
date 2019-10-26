(ns kand.type)

(defrecord Application [exps])
(defrecord If [pred t f])
(defrecord Def [name body])
(defrecord Lambda [params body])
(defrecord Primitive [f])
(defrecord Symbol [name])
(defrecord Num [val])
(defrecord Unit [])


