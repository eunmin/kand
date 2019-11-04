(ns kand.test.util
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [expound.alpha :as expound]))

(defn check [sym]
  (let [check-result (stest/check sym {:clojure.spec.test.check/opts {:max-size 3}})
        result (-> check-result
                   first
                   :clojure.spec.test.check/ret
                   :result)]
    (when-not (true? result)
      (binding [s/*explain-out* expound/printer]
        (expound/explain-results check-result)))
    result))
