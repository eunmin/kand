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
(defrecord Unit [])
(defrecord Err [message])

#_(

   (:: value Int)
   (def value 1)

   (:: 1 Int)

   (:: add (Int Int Int))
   (def add (x y)
     )

   (:: minus ((Num a) (Num b)) (a a b))
   (def minus (x y)
     )

   (:: append ((Int a)) ([a] a))
   (def apeend (xs x)
     )

   (:: foo ((Num a)) a)
   (def foo 1)

   ;; type alias
   (deftype GameValue Int)

   ;; product type
   (defrecord Person
     (:: first-name String)
     (:: last-name String))

   ;; sum type
   (defdata Profession
     (or Fighter Archer Accountant))

   (defclass (Monad m)
     (>>= ((m a) (a (m b)) (m b)))
     (return (a (m a))))

   (defclass ((Eq a)) (Num a)
     ((+ -) (a a a))
     (negate (a a)))

   )
