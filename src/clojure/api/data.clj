(ns clojure.api.data
  (:require [clojure.string :refer (join split)]
            [clojure.api.dsl :refer (convert defrule)]
            [clojure.api.bean :refer (getters flags)]
            [clojure.api.class-utils :refer (class-name)]))

;; converter function takes expected type from API and obj and returns data

(defrule java-map [java.util.Map ?k ?v] c
         (fn [obj]
           (->> (.entrySet obj)
                (map (juxt #(c ?k (.getKey %))) #(c ?v (.getValue %)))
                (into {}))))

         #_(let [[k v] (map converter [?k ?v])
               per-entry (juxt #(k (.getKey %)) #(v (.getValue %)))]
           (fn [obj] (->> (.entrySet obj) (map per-entry) (into {}))))

(defrule java-set [java.util.Set ?e] c
         #(into #{} (map (partial c ?e) (seq %))))

(defrule java-vector [java.util.Vector ?e] c
         #(mapv (partial c ?e) (seq %)))

(defrule java-iterable [Iterable ?e] c
         #(map (partial c ?e) (seq %)))

(defrule java-array [?e] c #(mapv (partial c ?e) (seq %)))

(defrule java-bool Boolean _ identity)
(defrule java-prim-bool Boolean/TYPE _ identity)

(defrule java-int Integer/TYPE _ identity)
(defrule java-double Double/TYPE _ identity)
(defrule java-float Float/TYPE _ identity)
(defrule java-long Long/TYPE _ identity)

(defrule java-number Number _ identity)

(defrule java-string String _ identity)

(defrule java-date java.util.Date _ identity)

(defrule java-object Object c #(c (class %) %))

(defrule java-enum [Enum _] _ #(keyword (str %)))

(def memo-getters (memoize getters))

(defn not-anonymous [name] (when-not (.contains name "$") name))

(defrule any-type ?t c
           (fn [obj]
             (merge
               {
                #_:type #_(-> (class-name ?t)
                           (split #"\.")
                           last
                           .toLowerCase
                           keyword)
                :object obj}
               ((memo-getters ?t) c obj)
               ((flags ?t) obj)
               )))

(def default-converter
  (convert [java-number java-string java-bool java-date java-enum
            java-int java-double java-float java-long java-prim-bool
            java-map java-set java-vector java-iterable java-array
            java-object
            any-type]))

#_  (def default-rules [java-map java-set java-object java-array any-type])

(defn java->clojure
  ([obj])
  ([obj type])
  ([obj type root path visited]))