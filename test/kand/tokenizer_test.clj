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
    (let [s "\"a b c\""
          buf ""
          result []
          result (tokenize-string s "" result)]
      (is (= "\"a b c\"" (first result)))))
  (testing "mismatch string"
    (let [s "\"a b c"
          buf ""
          result []]
      (is (thrown-with-msg? Exception #"Mismatched String" (tokenize-string s "" result)))))
  (testing "not string riteral"
    (let [s "x \"a b c\""
          buf ""
          result []]
      (is (thrown-with-msg? Exception #"Not String literal" (tokenize-string s "" result))))))

(deftest tokenize-test []
  (testing "Single string"
    (let [s "a"
          result (tokenize s "" [])]
      (is (= ["a"] (first result)))))
  (testing "Multi string"
    (let [s "abc"
          result (tokenize s "" [])]
      (is (= ["abc"] (first result)))))
  (testing "Trim spaces"
    (let [s "  abc  def  "
          result (tokenize s "" [])]
      (is (= ["abc" "def"] (first result)))))
  (testing "Parentheses"
    (let [s "(abc)"
          result (tokenize s "" [])]
      (is (= [["abc"]] (first result)))))
  (testing "Parentheses Nested"
    (let [s "(abc (def))"
          result (tokenize s "" [])]
      (is (= [["abc" ["def"]]] (first result)))))
  (testing "Parentheses Nested (No spaces)"
    (let [s "(abc(def))"
          result (tokenize s "" [])]
      (is (= [["abc" ["def"]]] (first result)))))
  (testing "Empty parentheses"
    (let [s "()"
          result (tokenize s "" [])]
      (is (= [[]] (first result)))))
  (testing "String"
    (let [s "\"a b c\""
          result (tokenize s "" [])]
      (is (= ["\"a b c\""] (first result)))))
  (testing "Quote literal"
    (let  [s "'a"
           result (tokenize s "" [])]
      (is (= [["quote" "a"]] (first result))))))

(deftest tokenize-error []
  (testing "Mismatched string"
    (let [s "\""]
      (is (thrown-with-msg? Exception #"Mismatched String" (tokenize s "" [])))))
  (testing "Mismatched parentheses"
    (let [s "("]
      (is (thrown-with-msg? Exception #"Mismatched parentheses" (tokenize s "" []))))))
