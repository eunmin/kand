(ns kand.analyzer
  (:refer-clojure :exclude [name])
  (:require [kand.type :refer :all]
            [clojure.string :refer [split]]
            [kand.parser :refer [parse]])
  (:import [kand.type Application If Def Lambda Primitive Symbol Num True False Unit Quote Err Module Import Eval]))

(defmulti analyze type)

(defmulti execute (fn [v _ _] (type v)))

(defmethod execute Lambda [{:keys [params body]} args env]
  (let [bproc (analyze body)
        module (:name (:core/*module* env))
        [result _] (bproc (merge env (zipmap (map #(keyword module (:name %))
                                                  params)
                                             args)))]
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
      (let [module (or module (:name (:core/*module* env)))]
        (if-let [value (get env (keyword module sym-name))]
          [value env]
          [(->Err (str "Can't find symbol " sym-name)) env])))))

(defmethod analyze Def [{:keys [name body]}]
  (let [bodyf (analyze body)]
    (fn analyze-def [env]
      (let [[result env] (bodyf env)
            module (:core/*module* env)]
        [(->Unit) (assoc env (keyword (:name module) (:name name)) result)]))))

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

(defmethod analyze Module [sym]
  (fn analyze-module [env]
    [(->Unit) (assoc env :core/*module* (:name sym))]))

(defn rename-keys-ns [m old-ns new-ns]
  (into {} (map #(vector (keyword new-ns (clojure.core/name %))
                         (% m))
                (filter #(= old-ns (namespace %)) (keys m)))))

(defmethod analyze Import [module]
  (fn analyze-import [env]
    (let [module-name (-> module :module :name)
          code (slurp (str module-name ".knd"))
          org-module (:core/*module* env)
          [result new-env] ((analyze (->Eval (->Str code))) env)]
      [result (-> new-env
                  (merge new-env (rename-keys-ns new-env
                                                 module-name
                                                 (:name org-module)))
                  (assoc :core/*module* org-module))])))

(defmethod analyze Eval [code]
  (fn analyze-eval [env]
    (try
      (let [result (reduce (fn [[_ env] exp]
                             ((analyze exp) env))
                           [nil env] (parse (-> code :code :val)))]
        [(first result) (second result)])
      (catch Exception e
        [(->Err (.getMessage e)) env]))))

(defmethod analyze :default [exp]
  (fn analyze-default [env]
    [exp env]))
