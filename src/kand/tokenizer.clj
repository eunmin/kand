(ns kand.tokenizer
  (:require [clojure.string :refer [blank?]]
            [cats.core :as m]
            [cats.monad.either :refer :all]))

(defn append-token [result token]
  (if (= "" token)
    result
    (conj result token)))

(defn tokenize-string [[x & xs] buf result]
  (if x
    (if (empty? buf)
      (if (= \" x)
        (tokenize-string xs (str buf x) result)
        (left {:message "Not String literal"}))
      (if (= \" x)
        (right [(str buf x) xs])
        (tokenize-string xs (str buf x) result)))
    (if (empty? buf)
      (right ["" xs])
      (left {:message "Mismatched String"}))))

(defmulti tokenize (fn [s _ _] s))

(defmethod tokenize nil [_ buf result]
  (right [(append-token result buf) ""]))

(defmethod tokenize :default [[x & xs :as s] buf result]
  (cond
    (= \( x) (m/bind (tokenize xs "" [])
                     (fn [[ast rst]]
                       (tokenize rst "" (append-token (append-token result buf) ast))))
    (= \) x) (right [(append-token result buf) xs])
    (= \" x) (m/bind (tokenize-string s "" result)
                     (fn [[s rst]]
                       (tokenize rst "" (append-token (append-token result buf) s))))
    (nil? x) (left {:message "Mismatched parentheses"})
    (blank? (str x)) (tokenize xs "" (append-token result buf))
    :else (tokenize xs (str buf x) result)))
