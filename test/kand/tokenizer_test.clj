(ns kand.tokenizer-test
  (:require [kand.tokenizer :refer :all]
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

(deftest tokenize-string-test []
  (testing "tokenize-string"
    (let [s "a b c\""
          buf ""
          result []
          [result rst] (tokenize-string s buf result)]
      (is (= "a b c\"" result)))))

(deftest tokenize-test []
  (testing "Single string"
    (let [s "a"
          [result _] (tokenize s "" [])]
      (is (= ["a"] result))))
  (testing "Multi string"
    (let [s "abc"
          [result _] (tokenize s "" [])]
      (is (= ["abc"] result))))
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
      (is (= [[]] result))))
  (testing "String"
    (let [s "\"a b c\""
          [result _] (tokenize s "" [])]
      (is (= ["\"a b c\""] result)))))

;; (deftest tokenize-error []
;;   (testing "Mismatched parentheses"
;;     (let [s "("
;;           [result _] (tokenize s "" [])]
;;       (is (contains? result :error))
;;       (is (= :mismatched-parentheses (get-in result [:error :type]))))))


