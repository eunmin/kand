(ns kand.parser-test
  (:require [kand.parser :refer :all]
            [kand.type :refer :all]
            [clojure.test :refer :all]))

(deftest append-token-test []
  (testing "append-token"
    (let [token "token"
          result []
          result (append-token result token)]
      (is (= ["token"] result))))
  (testing "append-token with empty string"
    (let [token ""
          result []
          result (append-token result token)]
      (is (= [] result)))))

(deftest tokenize-test []
  (testing "Single string"
    (let [s "a"
          [result _] (tokenize s "" [])]
      (is (= ["a"] result))))
  (testing "Multi string"
    (let [s "abc"
          [result _] (tokenize s "" [])]
      (is (= ["abc" result]))))
  (testing "Trim spaces"
    (let [s "  abc  def  "
          [result _] (tokenize s "" [])]
      (is (= ["abc" "def"] result))))
  (testing "Parentheses"
    (let [s "(abc)"
          [result _] (tokenize s "" [])]
      (is (= [["abc"]] result))))
  (testing "Parentheses Nested"
    (let [s "(abc (def))"
          [result _] (tokenize s "" [])]
      (is (= [["abc" ["def"]]] result))))
  (testing "Parentheses Nested (No spaces)"
    (let [s "(abc(def))"
          [result _] (tokenize s "" [])]
      (is (= [["abc" ["def"]]] result))))
  (testing "Empty parentheses"
    (let [s "()"
          [result _] (tokenize s "" [])]
      (is (= [[]] result)))))

(deftest parse-token-if []
  (let [s ["if" "a" "b" "c"]
        result (parse-token s)]
    (is (= (->If (->Symbol "a") (->Symbol "b") (->Symbol "c")) result))))

(deftest parse-token-def []
  (let [s ["def" "a" "1"]
        result (parse-token s)]
    (is (= (->Def (->Symbol "a") (->Num 1)) result))))

(deftest parse-token-fn []
  (testing "No parameters"
    (let [s ["fn" [] "1"]
          result (parse-token s)]
      (is (= (->Lambda [] (->Num 1)) result))))
  (testing "With parameters"
    (let [s ["fn" ["x" "y"] ["+" "x" "y"]]
          result (parse-token s)]
      (is (= (->Lambda [(->Symbol "x") (->Symbol "y")]
                       (->Application [(->Symbol "+")
                                       (->Symbol "x")
                                       (->Symbol "y")])))))))

(deftest parse-token-vector []
  (let [s ["+" "1" "2"]
        result (parse-token s)]
    (is (= (->Application [(->Symbol "+")
                           (->Num 1)
                           (->Num 2)])
           result))))

(deftest parse-token-number []
  (let [s "1"
        result (parse-token s)]
    (is (= (->Num 1) result))))

(deftest parse-token-symbol []
  (let [s "a"
        result (parse-token s)]
    (is (= (->Symbol "a") result))))

(deftest parse-token-true []
  (let [s "true"
        result (parse-token s)]
    (is (= (->True) result))))

(deftest parse-token-false []
  (let [s "false"
        result (parse-token s)]
    (is (= (->False) result))))

(deftest parse-test []
  (let [s "(def add (fn (x y) (+ 1 (+ x y))))"
        result (parse s)]
    (is (= [(->Def (->Symbol "add")
                    (->Lambda [(->Symbol "x") (->Symbol "y")]
                              (->Application [(->Symbol "+")
                                              (->Num 1)
                                              (->Application [(->Symbol "+")
                                                              (->Symbol "x")
                                                              (->Symbol "y")])])))]
           result))))
