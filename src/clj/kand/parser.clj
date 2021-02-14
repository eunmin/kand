(ns kand.parser
  (:require [kand.tokenizer :refer [tokenize]]
            [kand.type :refer :all]))

(defmulti parse-token (fn [token]
                        (when (vector? token)
                          (keyword (first token)))))

(defmethod parse-token :if [[_ pred t f & rst :as token]]
  (if rst
    (throw (ex-info "Too many arguments to if" {:token token}))
    (let [p (parse-token pred)
          t (parse-token t)
          f (parse-token f)]
      (->If p t f))))

(defmethod parse-token :def [[_ def-name body & rst :as token]]
  (if rst
    (throw (ex-info "Too many arguments to def" {:token token}))
    (if (vector? def-name)
      (let [[sym-token & args-token] def-name]
        (let [sym (parse-token sym-token)
              args (parse-token ["fn" (vec args-token) body])]
          (->Def sym args)))
      (let [sym (parse-token def-name)
            body (parse-token body)]
        (->Def sym body)))))

(defmethod parse-token :fn [[_ arg-tokens body-token & rst :as token]]
  (if rst
    (throw (ex-info "Too many arguments to fn" {:token token}))
    (if (vector? arg-tokens)
      (let [args (map parse-token arg-tokens)
            body (parse-token body-token)]
        (->Lambda args body))
      (throw (ex-info "Parameter declaration should be a vector" {:token token})))))

(defmethod parse-token :quote [[_ value]]
  (->Quote (parse-token value)))

(defmethod parse-token :module [[_ module-name]]
  (->Module (parse-token module-name)))

(defmethod parse-token :import [[_ module]]
  (->Import (parse-token module)))

(defmethod parse-token :eval [[_ code]]
  (->Eval (parse-token code)))

(defmethod parse-token :default [token]
  (cond
    (vector? token) (->Application (map parse-token token))
    (= "true" token) (->True)
    (= "false" token) (->False)
    (string? (read-string token)) (->Str (read-string token))
    (number? (read-string token)) (->Num (read-string token))
    :else (->Symbol token)))

(defn parse [s]
  (map parse-token (first (tokenize s "" []))))
