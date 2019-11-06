(ns kand.tokenizer
  (:require [clojure.string :refer [blank?]]))

(defn append-token [result token]
  (if (= "" token)
    result
    (conj result token)))

(defn tokenize-string [[x & xs] buf result]
  (if x
    (if (empty? buf)
      (if (= \" x)
        (tokenize-string xs (str buf x) result)
        [{:error {:type :not-string-literal}} xs])
      (if (= \" x)
        [(str buf x) xs]
        (tokenize-string xs (str buf x) result)))
    (if (empty? buf)
      ["" xs]
      [{:error {:type :mismatch-string}}])))

(defmulti tokenize (fn [s _ _] s))

(defmethod tokenize nil [_ buf result]
  [(append-token result buf) ""])

(defmethod tokenize :default [[x & xs :as s] buf result]
  (cond
    (= \( x) (let [[ast rst] (tokenize xs "" [])]
               (tokenize rst "" (append-token (append-token result buf) ast)))
    (= \) x) [(append-token result buf) xs]
    (= \" x) (let [[s rst] (tokenize-string s "" result)]
               (tokenize rst "" (append-token (append-token result buf) s)))
    (blank? (str x)) (tokenize xs "" (append-token result buf))
    :else (tokenize xs (str buf x) result)))
