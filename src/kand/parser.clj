(ns kand.parser
  (:require [kand.tokenizer :refer [tokenize]]
            [kand.type :refer :all]
            [cats.core :as m]
            [cats.context :as ctx]
            [cats.monad.either :refer :all :as either]))

(defmulti parse-token (fn [token]
                        (when (vector? token)
                          (keyword (first token)))))

(defmethod parse-token :if [[_ pred t f & rst]]
  (if rst
    (left {:message "Too many arguments to if"})
    (m/mlet [p (parse-token pred)
             t (parse-token t)
             f (parse-token f)]
            (m/return (->If p t f)))))

(defmethod parse-token :def [[_ def-name body & rst]]
  (if rst
    (left {:message "Too many arguments to def"})
    (if (vector? def-name)
      (let [[sym-token & args-token] def-name]
        (m/mlet [sym (parse-token sym-token)
                 args (parse-token ["fn" (vec args-token) body])]
                (m/return (->Def sym args))))
      (m/mlet [sym (parse-token def-name)
               body (parse-token body)]
              (m/return (->Def sym body))))))

(defmethod parse-token :fn [[_ arg-tokens body-token & rst]]
  (if rst
    (left {:message "Too many arguments to fn"})
    (if (vector? arg-tokens)
      (m/mlet [args (ctx/with-context either/context
                      (m/mapseq parse-token arg-tokens))
               body (parse-token body-token)]
        (m/return (->Lambda args body)))
      (left {:message "Parameter declaration should be a vector"}))))

(defmethod parse-token :quote [[_ value]]
  (m/fmap #(->Quote %) (parse-token value)))

(defmethod parse-token :module [[_ module-name]]
  (m/fmap #(->Module %) (parse-token module-name)))

(defmethod parse-token :import [[_ module]]
  (m/fmap #(->Import %) (parse-token module)))

(defmethod parse-token :eval [[_ code]]
  (m/fmap #(->Eval %) (parse-token code)))

(defmethod parse-token :default [token]
  (cond
    (vector? token) (m/fmap #(->Application %) (m/mapseq parse-token token))
    (= "true" token) (right (->True))
    (= "false" token) (right (->False))
    (string? (read-string token)) (right (->Str (read-string token)))
    (number? (read-string token)) (right (->Num (read-string token)))
    :else (right (->Symbol token))))

(defn parse [s]
  (m/bind (tokenize s "" []) #(m/mapseq parse-token (first %)) ))

