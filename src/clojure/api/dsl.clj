(ns clojure.api.dsl
  (:require [clojure.api.class-utils :refer [all-types]]
            [clojure.core.match :refer [match]]
            [clojure.set :refer [union]])
  (:import (java.lang.reflect Type ParameterizedType GenericArrayType)))

(defn single? [coll] (and
                       (coll? coll)
                       (not-empty coll)
                       (empty? (rest coll))))

;; return a function which gets given a lookup function
;; the lookup function is called and returns functions
(defrecord Loop [path obj])

(defn map-values [f m] (zipmap (keys m) (map f (vals m))))

(defn best-match
  "Returns the applied rule which matches the type best"
  [rules type]
  (->>
    (for [rule rules subtype (all-types type)]
      (rule subtype))
    (filter identity)
    first))

(def best-match-memo (memoize best-match))

(defn convert
  "Specialize function takes two parameters: [api-type actual-type] and returns
   a collection of types"
  ([rules] (convert rules (fn [api-type actual-type] #{api-type})))
  ([rules specialize] (fn [type] (fn [obj] (convert rules specialize type [] obj))))
  ([rules specialize type path obj]
    (if (some (partial = obj) path)
      (Loop. path obj)

      (let [converter (fn [type new-obj]
                        (convert rules specialize type (conj path obj) new-obj))
            result (fn [^Type type] (((best-match-memo rules type) converter) obj))]

        (let [types (specialize type (class obj))]
          (if (single? types)
            (result (first types))
            (let [default (result type)]
              (cond
                (map? default) (apply merge default (filter map? (map result types)))
                (set? default) (apply union default (filter set? (map result types)))
                :else default))))))))


(defn form-seq [form] (tree-seq coll? seq form))

(defn symbols? [form] (->> form form-seq (filter symbol?) distinct))

(defn variable? [symbol] (and (symbol? symbol) (= \? (first (str symbol)))))


(defn merge-matches [& matches]
  (when (every? (comp not nil?) matches)
    (let [merged (apply merge-with #(when (= %1 %2) %1) matches)]
      (when (every? (comp not nil?) (vals merged)) merged))))

(defn matcher
  "Returns map of symbol -> type, empty map if match occured but no symbols to bind,
   and nil if didn't match"
  [pattern ^Type type]
  (cond
    (variable? pattern) {pattern type}

    (contains? #{'_ type} pattern) {}

    :else
    (condp instance? type
      Class
      (cond
        (and (instance? Class pattern) (= (.getName pattern) (.getName type))) {}

        (and (.isArray type) (vector? pattern) (single? pattern))
        (matcher (first pattern) (.getComponentType type)))

      ParameterizedType
      (when (vector? pattern)
        (let [types (cons (.getRawType type) (.getActualTypeArguments type))]
          (when (= (count types) (count pattern))
            (apply merge-matches (map matcher pattern types)))))

      GenericArrayType (when (and (vector? pattern) (single? pattern))
                         (matcher (first pattern) (.getGenericComponentType type)))
      )))

(defn quote-vars [form]
  (clojure.walk/postwalk #(if (or (= '_ %) (variable? %)) `'~% %) form))

(defmacro rule
  "A rule is a function which takes a Type and returns a function which takes a converter or returns:
     a data function or nil if the rule doesn't match"
  [pattern converter body]
  `(let [f# (fn [~converter {:syms ~(vec (filter variable? (symbols? pattern)))}] ~body)
         pattern# ~(quote-vars pattern)]
     (fn [^Type type#]
       (when-let [matched-types# (matcher pattern# type#)]
         (fn [converter#]
           (f# converter# matched-types#)
           )))))

(defmacro rules [pattern converter body & triples]
  (if triples
    `(cons (rule ~pattern ~converter ~body) (rules ~@triples))
    `(list (rule ~pattern ~converter ~body))
    ))

(defmacro defrule [name pattern converter body] `(def ~name (rule ~pattern ~converter ~body)))

(defmacro defrules [name & triples]
  `(def ~name (rules ~@triples)))
