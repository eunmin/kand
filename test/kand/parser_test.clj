(ns kand.parser-test
  (:require [clojure.test :refer :all]
            [clojure.spec.gen.alpha :as gen]
            [kand.parser :refer :all :as parser]
            [kand.type :refer :all]
            [cats.monad.either :refer :all]))

(deftest parse-token-if []
  (let [s ["if" "a" "b" "c"]
        result (parse-token s)]
    (is (right? result))
    (is (= (->If (->Symbol "a") (->Symbol "b") (->Symbol "c")) (:right result)))))

(deftest parse-token-if-error []
  (let [s ["if" "a" "b" "c" "d"]
        result (parse-token s)]
    (is (left? result))))

(deftest parse-token-def []
  (let [s ["def" "a" "1"]
        result (parse-token s)]
    (is (right? result))
    (is (= (->Def (->Symbol "a") (->Num 1)) (:right result)))))

(deftest parse-token-def-function []
  (let [s ["def" ["a" "x"] "x"]
        result (parse-token s)]
    (is (right? result))
    (is (= (->Def (->Symbol "a") (->Lambda [(->Symbol "x")]
                                           (->Symbol "x"))) (:right result)))))

(deftest parse-token-def-error []
  (let [s ["def" "a" "1" "2" "3"]
        result (parse-token s)]
    (is (left? result))))

(deftest parse-token-fn []
  (testing "No parameters"
    (let [s ["fn" [] "1"]
          result (parse-token s)]
      (is (right? result))  
      (is (= (->Lambda [] (->Num 1)) (:right result)))))
  (testing "With parameters"
    (let [s ["fn" ["x" "y"] ["+" "x" "y"]]
          result (parse-token s)]
      (is (right? result))
      (is (= (->Lambda [(->Symbol "x") (->Symbol "y")]
                       (->Application [(->Symbol "+")
                                       (->Symbol "x")
                                       (->Symbol "y")]))
             (:right result))))))

(deftest parse-token-fn-error []
  (testing "Too many args"
    (let [s ["fn" [] "1" "2"]
          result (parse-token s)]
      (is (left? result))))
  (testing "Non vector parameters"
    (let [s ["fn" "a" "1" "2"]
          result (parse-token s)]
      (is (left? result)))))

(deftest parse-token-vector []
  (let [s ["+" "1" "2"]
        result (parse-token s)]
    (is (right? result))
    (is (= (->Application [(->Symbol "+")
                           (->Num 1)
                           (->Num 2)])
           (:right result)))))

(deftest parse-token-number []
  (let [s "1"
        result (parse-token s)]
    (is (right? result))
    (is (= (->Num 1) (:right result)))))

(deftest parse-token-symbol []
  (let [s "a"
        result (parse-token s)]
    (is (right? result))
    (is (= (->Symbol "a") (:right result)))))

(deftest parse-token-true []
  (let [s "true"
        result (parse-token s)]
    (is (right? result))
    (is (= (->True) (:right result)))))

(deftest parse-token-false []
  (let [s "false"
        result (parse-token s)]
    (is (right? result))
    (is (= (->False) (:right result)))))

(deftest parse-test []
  (let [s "(def add (fn (x y) (+ 1 (+ x y))))"
        result (parse s)]
    (is (right? result))
    (is (= [(->Def (->Symbol "add")
                    (->Lambda [(->Symbol "x") (->Symbol "y")]
                              (->Application [(->Symbol "+")
                                              (->Num 1)
                                              (->Application [(->Symbol "+")
                                                              (->Symbol "x")
                                                              (->Symbol "y")])])))]
           (:right result)))))
