(ns kand.main
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require [clojure.string :refer [split]]
            [kand.parser :refer [parse]]
            [kand.analyzer :refer [analyze]]
            [kand.core :refer [core-env]]
            [cats.core :as m]
            [cats.monad.either :refer :all]))

(defn eval [exp env]
  ((analyze exp) env))

(defn repl [env s]
  (print (str (:name (:core/*module* env)) "> "))
  (flush)
  (let [line (read-line)
        [command & args] (split line #"\s+")]
    (case command
      ":quit" (println "Bye See you soon!")
      (let [result (m/fmap #(reduce (fn [[_ env] exp]
                                       (eval exp env))
                                     [nil env] %)
                           (parse (str s line)))]
        (if (right? result)
          (println (first (:right result)))
          (let [message (-> result :left :message)]
            (if (or (= message "Mismatched String")
                    (= message "Mismatched parentheses"))
              (repl env (str line "\n"))
              (println "Error: " message))))
        (repl (second (:right result)) "")))))

(defn -main [& args]
  (println "Kand REPL\nTo exit type :quit")
  (repl core-env ""))
