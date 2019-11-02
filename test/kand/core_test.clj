(ns kand.core-test
  (:require [kand.core :refer :all]
            [kand.type :refer :all]
            [clojure.test :refer :all]))

(deftest new-object-test
  (testing "without args"
    (let [cls (->Symbol "java.util.Random")
          result (new-object cls)]
      (is (instance? java.util.Random (:obj result)))))
  (testing "with args"
    (let [cls (->Symbol "java.util.Random")
          arg (->Num 100)
          result (new-object cls arg)]
      (is (instance? java.util.Random (:obj result))))))

(deftest invoke-method-test
  (testing "without args"
    (let [obj (new-object (->Symbol "java.util.Random"))
          result (invoke-method (->Symbol "nextInt") obj)]
      (is (number? (:obj result))))))
