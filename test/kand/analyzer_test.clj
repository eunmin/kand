(ns kand.analyzer-test
  (:require [kand.analyzer :refer :all]
            [kand.type :refer :all]
            [clojure.test :refer :all])
  (:import kand.type.Err))

(deftest execute-primitive []
  (let [exp (->Primitive identity)
        env {}
        [result new-env] (execute exp [(->Num 1)] env)]
    (is (= (->Num 1) result))
    (is (= env new-env))))

(deftest execute-lambda []
  (let [exp (->Lambda [(->Symbol "x") (->Symbol "y")]
                      (->Application
                       [(->Symbol "+") (->Symbol "x") (->Symbol "y")]))
        env {"+" (->Primitive (fn [{x :val} {y :val}] (->Num (+ x y))))}
        [result new-env] (execute exp [(->Num 1) (->Num 2)] env)]
    (is (= (->Num 3) result))
    (is (= env new-env))))

(deftest execute-default []
  (let [exp (->Symbol "a")
        env {}
        [result new-env] (execute exp [] env)]
    (is (instance? Err result))
    (is (= env new-env))))

(deftest analyze-application []
  (testing "Application"
    (let [exp (->Application [(->Primitive identity) (->Num 1)])
          env {}
          [result new-env] ((analyze exp) env)]
      (is (= (->Num 1) result))
      (is (= env new-env))))
  (testing "Nested Application"
    (let [exp (->Application [(->Primitive identity)
                              (->Application [(->Primitive identity)
                                              (->Num 1)])])
          env {}
          [result new-env] ((analyze exp) env)]
      (is (= (->Num 1) result))
      (is (= env new-env)))))

(deftest analyze-symbol []
  (let [exp (->Symbol "a")
        env {"a" (->Num 1)}
        [result new-env] ((analyze exp) env)]
    (is (= result (->Num 1)))
    (is (= env new-env))))

(deftest analyze-def []
  (testing "Def"
    (let [exp (->Def (->Symbol "a") (->Num 1))
          env {}
          [result new-env] ((analyze exp) env)]
      (is (= (->Num 1) (get new-env "a")))))
  (testing "Def expression"
    (let [exp (->Def (->Symbol "a") (->Application [(->Primitive identity)
                                                    (->Num 1)]))
          env {}
          [result new-env] ((analyze exp) env)]
      (is (= (->Num 1) (get new-env "a"))))))

(deftest analyze-if []
  (testing "True"
    (let [exp (->If (->True) (->Num 1) (->Num 2))
          env {}
          [result new-env] ((analyze exp) env)]
      (is (= (->Num 1) result))
      (is (= env new-env))))
  (testing "False"
    (let [exp (->If (->False) (->Num 1) (->Num 2))
          env {}
          [result new-env] ((analyze exp) env)]
      (is (= (->Num 2) result))
      (is (= env new-env)))))

(deftest analyze-default []
  (let [exp (->Num 1)
        env {}
        [result new-env] ((analyze exp) env)]
    (is (= (->Num 1) result))
    (is (= env new-env))))
