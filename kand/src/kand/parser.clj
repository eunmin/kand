(ns kand.parser
  (:require [clojure.string :refer [blank?]]
            [kand.type :refer :all]))

(defn append-token [result token]
  (if (= "" token)
    result
    (conj result token)))

(defmulti tokenize (fn [s _ _] s))

(defmethod tokenize nil [_ buf result]
  [(append-token result buf) ""])

(defmethod tokenize :default [[x & xs :as s] buf result]
  (cond
    (= \( x) (let [[ast rst] (tokenize xs "" [])]
               (tokenize rst "" (append-token result ast)))
    (= \) x) [(append-token result buf) xs]
    (blank? (str x)) (tokenize xs "" (append-token result buf))
    :else (tokenize xs (str buf x) result)))

(defmulti parse-token (fn [token]
                        (when (vector? token)
                          (keyword (first token)))))

(defmethod parse-token :if [[_ pred t f & rst]]
  ;; check! rst must nil!!
  (->If (parse-token pred) (parse-token t) (parse-token f)))

(defmethod parse-token :def [[_ sym body]]
  (->Def (parse-token sym) (parse-token body)))

(defmethod parse-token :fn [[_ args body]]
  (->Lambda (map parse-token args) (parse-token body)))

(defmethod parse-token :default [token]
  (cond
    (vector? token) (->Application (map parse-token token))
    (number? (read-string token)) (->Num (read-string token))
    :else (->Symbol token)))

(defn parse [s]
  (let [[tokens] (tokenize s "" [])]
    (map parse-token tokens)))

(parse "(def add (fn (x y) 
                   (+ x y)))")
