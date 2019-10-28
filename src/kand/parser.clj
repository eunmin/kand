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
               (tokenize rst "" (append-token (append-token result buf) ast)))
    (= \) x) [(append-token result buf) xs]
    (blank? (str x)) (tokenize xs "" (append-token result buf))
    :else (tokenize xs (str buf x) result)))

(defmulti parse-token (fn [token]
                        (when (vector? token)
                          (keyword (first token)))))

(defmethod parse-token :if [[_ pred t f & rst]]
  (if rst
    (->Err "Too many arguments to if")
    (->If (parse-token pred) (parse-token t) (parse-token f))))

(defmethod parse-token :def [[_ sym body & rst]]
  (if rst
    (->Err "Too many arguments to def")
    (->Def (parse-token sym) (parse-token body))))

(defmethod parse-token :fn [[_ args body & rst]]
  (if rst
    (->Err "Too many arguments to fn")
    (if (vector? args)
      (->Lambda (map parse-token args) (parse-token body))
      (->Err "Parameter declaration should be a vector"))))

(defmethod parse-token :default [token]
  (cond
    (vector? token) (->Application (map parse-token token))
    (= "true" token) (->True)
    (= "false" token) (->False)
    (number? (read-string token)) (->Num (read-string token))
    :else (->Symbol token)))

(defn parse [s]
  (let [[tokens] (tokenize s "" [])]
    (map parse-token tokens)))