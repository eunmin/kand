(ns kand.main
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require [clojure.string :refer [split]]
            [kand.parser :refer [parse]]
            [kand.analyzer :refer [analyze]]
            [kand.core :refer [core-env]]))

(defn eval [exp env]
  ((analyze exp) env))

(defn repl [env s]
  (print (str (:name (:core/*module* env)) "> "))
  (flush)
  (let [line (read-line)
        [command & args] (split line #"\s+")]
    (case command
      ":quit" (println "Bye See you soon!")
      (try
        (let [result (reduce (fn [[_ env] exp]
                               (eval exp env))
                             [nil env]
                             (parse (str s line)))]
          (println (first result))
          (repl (second result) ""))
        (catch Exception e
          (let [message (.getMessage e)]
            (if (or (= message "Mismatched String")
                    (= message "Mismatched parentheses"))
              (repl env (str line "\n"))
              (do (println "Error: " message)
                  (repl nil "")))))))))

(defn -main [& args]
  (println "Kand REPL\nTo exit type :quit")
  (repl core-env ""))
