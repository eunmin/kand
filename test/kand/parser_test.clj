(ns kand.parser-test
  (:require [clojure.test :refer :all]
            [clojure.spec.gen.alpha :as gen]
            [kand.parser :refer :all :as parser]
            [kand.type :refer :all]
            [cats.monad.either :refer :all]))

(deftest parse-token-if-test []
  (let [s ["if" "a" "b" "c"]
        result (parse-token s)]
    (is (right? result))
    (is (= (->If (->Symbol "a") (->Symbol "b") (->Symbol "c")) (:right result)))))

(deftest parse-token-if-error-test []
  (let [s ["if" "a" "b" "c" "d"]
        result (parse-token s)]
    (is (left? result))))

(deftest parse-token-def-test []
  (let [s ["def" "a" "1"]
        result (parse-token s)]
    (is (right? result))
    (is (= (->Def (->Symbol "a") (->Num 1)) (:right result)))))

(deftest parse-token-def-function-test []
  (let [s ["def" ["a" "x"] "x"]
        result (parse-token s)]
    (is (right? result))
    (is (= (->Def (->Symbol "a") (->Lambda [(->Symbol "x")]
                                           (->Symbol "x"))) (:right result)))))

(deftest parse-token-def-error-test []
  (let [s ["def" "a" "1" "2" "3"]
        result (parse-token s)]
    (is (left? result))))

(deftest parse-token-fn-test []
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

(deftest parse-token-fn-error-test []
  (testing "Too many args"
    (let [s ["fn" [] "1" "2"]
          result (parse-token s)]
      (is (left? result))))
  (testing "Non vector parameters"
    (let [s ["fn" "a" "1" "2"]
          result (parse-token s)]
      (is (left? result)))))

(deftest parse-token-vector-test []
  (let [s ["+" "1" "2"]
        result (parse-token s)]
    (is (right? result))
    (is (= (->Application [(->Symbol "+")
                           (->Num 1)
                           (->Num 2)])
           (:right result)))))

(deftest parse-token-number-test []
  (let [s "1"
        result (parse-token s)]
    (is (right? result))
    (is (= (->Num 1) (:right result)))))

(deftest parse-token-symbol-test []
  (let [s "a"
        result (parse-token s)]
    (is (right? result))
    (is (= (->Symbol "a") (:right result)))))

(deftest parse-token-true-test []
  (let [s "true"
        result (parse-token s)]
    (is (right? result))
    (is (= (->True) (:right result)))))

(deftest parse-token-false-test []
  (let [s "false"
        result (parse-token s)]
    (is (right? result))
    (is (= (->False) (:right result)))))

(deftest parse-token-moudle-test []
  (let [s ["module" "user"]
        result (parse-token s)]
    (is (right? result))
    (is (= (->Module (->Symbol "user")) (:right result)))))

(deftest parse-token-import-test []
  (let [s ["import" "user"]
        result (parse-token s)]
    (is (right? result))
    (is (= (->Import (->Symbol "user")) (:right result)))))

(deftest parse-token-import-test []
  (let [s ["eval" "\"(+ 1 2)\""]
        result (parse-token s)]
    (is (right? result))
    (is (= (->Eval (->Str "(+ 1 2)")) (:right result)))))

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
