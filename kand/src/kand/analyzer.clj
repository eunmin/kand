(ns kand.analyzer
  (:refer-clojure :exclude [name])
  (:require [kand.type :refer :all])
  (:import [kand.type Application If Def Lambda Primitive Symbol Num True False Unit Err]))

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
    #(let [[app] (fproc %)
           args (map first (map (fn [aproc] (aproc %)) aprocs))]
       (execute app args %))))

(defmethod analyze Symbol [{:keys [name]}]
  #(vector (get % name) %))

(defmethod analyze Def [{:keys [name body]}]
  #(vector (->Unit) (assoc % (:name name) body)))

(defmethod analyze If [{:keys [pred t f]}]
  (let [pproc (analyze pred)
        tproc (analyze t)
        fproc (analyze f)]
    #(let [[p] (pproc %)]
       (if (= (->True) p)
         (tproc %)
         (fproc %)))))

(defmethod analyze :default [exp]
  #(vector exp %))


