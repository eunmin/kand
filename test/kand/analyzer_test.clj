(ns kand.analyzer-test
  (:require [kand.analyzer :refer :all]
            [kand.type :refer :all]
            [clojure.test :refer :all])
  (:import kand.type.Err))

(deftest execute-primitive-test []
  (let [exp (->Primitive identity)
        env {}
        [result new-env] (execute exp [(->Num 1)] env)]
    (is (= (->Num 1) result))
    (is (= env new-env))))

(deftest execute-lambda-test []
  (let [exp (->Lambda [(->Symbol "x") (->Symbol "y")]
                      (->Application
                       [(->Symbol "+") (->Symbol "x") (->Symbol "y")]))
        env {:core/*module* (->Symbol "user")
             :user/+ (->Primitive (fn [{x :val} {y :val}] (->Num (+ x y))))}
        [result new-env] (execute exp [(->Num 1) (->Num 2)] env)]
    (is (= (->Num 3) result))
    (is (= env new-env))))

(deftest execute-default []
  (let [exp (->Symbol "a")
        env {}
        [result new-env] (execute exp [] env)]
    (is (instance? Err result))
    (is (= env new-env))))

(deftest analyze-application-test []
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
        env {:core/*module* (->Symbol "user")
             :user/a (->Num 1)}
        [result new-env] ((analyze exp) env)]
    (is (= result (->Num 1)))
    (is (= env new-env))))

(deftest analyze-def-test []
  (testing "Def"
    (let [exp (->Def (->Symbol "a") (->Num 1))
          env {:core/*module* (->Symbol "user")}
          [result new-env] ((analyze exp) env)]
      (is (= (->Num 1) (get new-env :user/a)))))
  (testing "Def expression"
    (let [exp (->Def (->Symbol "a") (->Application [(->Primitive identity)
                                                    (->Num 1)]))
          env {:core/*module* (->Symbol "user")}
          [result new-env] ((analyze exp) env)]
      (is (= (->Num 1) (get new-env :user/a))))))

(deftest analyze-if-test []
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

(deftest analyze-module-test []
  (let [exp (->Module (->Symbol "user"))
        env {}
        [result new-env] ((analyze exp) env)]
    (is (= (->Symbol "user") (:core/*module* new-env)))))

(deftest analyze-import-test []
  (let [exp (->Import (->Symbol "test"))
        env {:core/*module* (->Symbol "user")}
        [_ new-env] ((analyze exp) env)]
    (is (= (->Symbol "user") (:core/*module* new-env)))
    (is (= (->Num 1) (:test/a new-env)))))

(deftest analyze-eval-test []
  (let [exp (->Eval (->Str "(+ 1 2)"))
        env {:core/*module* (->Symbol "user")
             :user/+ (->Primitive (fn [{x :val} {y :val}] (->Num (+ x y))))}
        [result new-env] ((analyze exp) env)]
    (is (not (nil? (:core/*module* new-env))))
    (is (= (->Num 3) result))))

(deftest analyze-default-test []
  (let [exp (->Num 1)
        env {}
        [result new-env] ((analyze exp) env)]
    (is (= (->Num 1) result))
    (is (= env new-env))))
