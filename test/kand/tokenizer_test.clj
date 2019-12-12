(ns kand.tokenizer-test
  (:require [kand.tokenizer :refer :all]
            [clojure.test :refer :all]
            [cats.monad.either :refer :all]))

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
          {:keys [right]} (tokenize-string s "" result)]
      (is (= "\"a b c\"" (first right)))))
  (testing "mismatch string"
    (let [s "\"a b c"
          buf ""
          result []
          {:keys [left]} (tokenize-string s "" result)]
      (is (= "Mismatched String" (:message left)))))
  (testing "not string riteral"
    (let [s "x \"a b c\""
          buf ""
          result []
          {:keys [left]} (tokenize-string s "" result)]
      (is (= "Not String literal" (:message left))))))

(deftest tokenize-test []
  (testing "Single string"
    (let [s "a"
          {:keys [right]} (tokenize s "" [])]
      (is (= ["a"] (first right)))))
  (testing "Multi string"
    (let [s "abc"
          {:keys [right]} (tokenize s "" [])]
      (is (= ["abc"] (first right)))))
  (testing "Trim spaces"
    (let [s "  abc  def  "
          {:keys [right]} (tokenize s "" [])]
      (is (= ["abc" "def"] (first right)))))
  (testing "Parentheses"
    (let [s "(abc)"
          {:keys [right]} (tokenize s "" [])]
      (is (= [["abc"]] (first right)))))
  (testing "Parentheses Nested"
    (let [s "(abc (def))"
          {:keys [right]} (tokenize s "" [])]
      (is (= [["abc" ["def"]]] (first right)))))
  (testing "Parentheses Nested (No spaces)"
    (let [s "(abc(def))"
          {:keys [right]} (tokenize s "" [])]
      (is (= [["abc" ["def"]]] (first right)))))
  (testing "Empty parentheses"
    (let [s "()"
          {:keys [right]} (tokenize s "" [])]
      (is (= [[]] (first right)))))
  (testing "String"
    (let [s "\"a b c\""
          {:keys [right]} (tokenize s "" [])]
      (is (= ["\"a b c\""] (first right)))))
  (testing "Quote literal"
    (let  [s "'a"
           {:keys [right]} (tokenize s "" [])]
      (is (= [["quote" "a"]] (first right))))))

(deftest tokenize-error []
  (testing "Mismatched string"
    (let [s "\""
          {:keys [left]} (tokenize s "" [])]
      (is (= "Mismatched String" (:message left)))))
  (testing "Mismatched parentheses"
    (let [s "("
          {:keys [left]} (tokenize s "" [])]
      (is (= "Mismatched parentheses" (:message left))))))


