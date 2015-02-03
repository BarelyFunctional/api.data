(ns clojure.api.bean
  (:require [clojure.api.class-utils :refer [specialize-type var->param]]
            [clojure.algo.generic.functor :refer [fmap]])
  (:import (java.lang.reflect Method Type ParameterizedType)))

(defn- has-prefix? [^String prefix ^String name]
  (and (.startsWith name prefix)
       (> (count name) (count prefix))
       (let [next (.charAt name (count prefix))]
         (or (Character/isUpperCase next) (Character/isDigit next)))))

(defn getter-name? [^String name]
  (and (not= name "getClass") (has-prefix? "get" name)))

(defn flag-name? [^String name]
  (or (has-prefix? "has" name) (has-prefix? "is" name)))

(defn zero-arity? [^Method method] (->> method .getParameterTypes alength zero?))

(defn fix-name [^String name]
  (let [to-drop (cond (.startsWith name "get") 3 (.startsWith name "is") 2 (.startsWith name "has") 3)]
    (apply str (cons (Character/toLowerCase (.charAt name to-drop)) (drop (inc to-drop) name)))))

(defn invoker [^Method method] (fn [obj]
                                 (.setAccessible method true)
                                 (.invoke method obj (into-array Object []))))

(defn method-name [^Method method] (.getName method))

(defn generic-return-type [^Method method] (.getGenericReturnType method))

;; "Returns list of [method type] tuples - of {:keyword :accessor :return-type} maps"
(defn properties [^Type type]
  (condp instance? type
    Class (->> type .getMethods
               (filter zero-arity?)
               (map (juxt method-name invoker generic-return-type))
               (map (partial zipmap [:name :accessor :return-type])))

    ParameterizedType

    (let [specializations (var->param type)]
      (map
        #(update-in % [:return-type] (partial specialize-type specializations))
        (properties (.getRawType type))))))

(defn key-fn [^String name]
  (when (getter-name? name)
    (keyword (fix-name name))
    ))

(defn flag-fn [^String name]
  (when (flag-name? name)
    (keyword (str (fix-name name) \?))))

(defn getters
  ([^Type type] (getters key-fn type))
  ([key-fn ^Type type]
    (let [template (->> (properties type)
                        (keep (fn [m] (when-let [key (key-fn (:name m))]
                                        [key (dissoc m :name)])))
                        (into {}))]
      (fn [converter obj]
        (fmap (fn [{:keys [accessor return-type]}]
                 (converter return-type (accessor obj))) template)))))

(defn flags
  ([^Type type] (flags flag-fn type))
  ([key-fn ^Type type]
    (let [template (->> (properties type)
                        (filter #(contains? #{Boolean Boolean/TYPE} (:return-type %)))
                        (keep (fn [m] (when-let [key (key-fn (:name m))]
                                        [key (:accessor m)])))
                        (into {}))]
      (fn [obj]
        (fmap (fn [accessor] (accessor obj)) template)))))