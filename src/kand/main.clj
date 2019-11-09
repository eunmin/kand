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

(defn repl [env]
  (print "> ")
  (flush)
  (let [line (read-line)
        [command & args] (split line #"\s+")]
    (case command
      ":quit" (println "Bye See you soon!")
      (let [result (m/fmap #(reduce (fn [[_ env] exp]
                                       (eval exp env))
                                     [nil env] %)
                           (parse line))]
        (if (right? result)
          (println (first (:right result)))
          (println "Error: " (-> result :left :message)))
        (repl (second result))))))

(defn -main [& args]
  (println "Kand REPL\nTo exit type :quit")
  (repl core-env))
