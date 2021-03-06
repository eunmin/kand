(ns kand.type)

(defrecord Application [exps])
(defrecord If [pred t f])
(defrecord Def [name body])
(defrecord Lambda [params body])
(defrecord Primitive [f])
(defrecord Symbol [name])
(defrecord Num [val])
(defrecord Str [val])
(defrecord True [])
(defrecord False [])
(defrecord Obj [obj])
(defrecord Quote [val])
(defrecord Unit [])
(defrecord Err [message])
(defrecord Module [name])
(defrecord Eval [code])
(defrecord Import [module])
