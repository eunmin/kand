(ns kand.analyzer
  (:refer-clojure :exclude [name])
  (:require [kand.type :refer :all]
            [clojure.string :refer [split]])
  (:import [kand.type Application If Def Lambda Primitive Symbol Num True False Unit Quote Err Module]))

(defmulti analyze type)

(defmulti execute (fn [v _ _] (type v)))

(defmethod execute Lambda [{:keys [params body]} args env]
  (let [bproc (analyze body)
        [result _] (bproc (merge env (zipmap (map :name params) args)))]
    [result env]))

(defmethod execute Primitive [{:keys [f]} args env]
  [(apply f args) env])

(defmethod execute :default [v _ env]
  [(->Err (str v " can't execute")) env])

(defmethod analyze Application [{:keys [exps]}]
  (let [[operator & operands] exps
        fproc (analyze operator)
        aprocs (map analyze operands)]
    (fn analyze-application [env]
      (let [[app] (fproc env)
            args (map first (map (fn [aproc] (aproc env)) aprocs))
            errs (filter #(instance? Err %) args)]
        (if (empty? errs)
          (execute app args env)
          [errs env])))))

(defmethod analyze Symbol [sym]
  (let [[m n] (split (:name sym) #"/")
        module (when n m)
        sym-name (if n n m)]
    (fn analyze-symbol [env]
      (if-let [value (get env sym-name)]
        [value env]
        [(->Err (str "Can't find symbol " sym-name)) env]))))

(defmethod analyze Def [{:keys [name body]}]
  (let [bodyf (analyze body)]
    (fn analyze-def [env]
      (let [[result env] (bodyf env)
            *current-module* (:*current-module* env (->Symbol "user"))]
        [(->Unit) (assoc env (:name name) result)]))))

(defmethod analyze If [{:keys [pred t f]}]
  (let [pproc (analyze pred)
        tproc (analyze t)
        fproc (analyze f)]
    (fn analyze-if [env]
      (let [[p] (pproc env)]
        (if (= (->True) p)
          (tproc env)
          (fproc env))))))

(defmethod analyze Quote [value]
  (fn analyze-quote [env]
    [(:val value) env]))

(defmethod analyze Module [module-name]
  (fn analyze-module [env]
    [(->Unit) (assoc env :*current-module* (:name module-name))]))

(defmethod analyze :default [exp]
  (fn analyze-default [env]
    [exp env]))


