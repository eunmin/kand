(ns kand.parser
  (:require [kand.tokenizer :refer [tokenize]]
            [kand.type :refer :all]
            [cats.monad.either :refer :all]))

(defmulti parse-token (fn [token]
                        (when (vector? token)
                          (keyword (first token)))))

(defmethod parse-token :if [[_ pred t f & rst]]
  (if rst
    (->Err "Too many arguments to if")
    (->If (parse-token pred) (parse-token t) (parse-token f))))


(defmethod parse-token :def [[_ def-name body & rst]]
  (if rst
    (->Err "Too many arguments to def")
    (if (vector? def-name)
      (let [[sym & args] def-name]
        (->Def (parse-token sym)
               (parse-token ["fn" (vec args) body])))
      (->Def (parse-token def-name) (parse-token body)))))

(defmethod parse-token :fn [[_ args body & rst]]
  (if rst
    (->Err "Too many arguments to fn")
    (if (vector? args)
      (->Lambda (map parse-token args) (parse-token body))
      (->Err "Parameter declaration should be a vector"))))

(defmethod parse-token :quote [[_ value]]
  (->Quote (parse-token value)))

(defmethod parse-token :default [token]
  (cond
    (vector? token) (->Application (map parse-token token))
    (= "true" token) (->True)
    (= "false" token) (->False)
    (string? (read-string token)) (->Str (read-string token))
    (number? (read-string token)) (->Num (read-string token))
    :else (->Symbol token)))

(defn parse [s]
  (let [value (tokenize s "" [])]
    (if (right? value)
      (map parse-token (first (:right value)))
      (->Err (str "tokenizer error" (:left value))))))

