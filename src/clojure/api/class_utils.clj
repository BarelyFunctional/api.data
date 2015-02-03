(ns clojure.api.class-utils
  (:import [java.lang.reflect Type ParameterizedType GenericArrayType]
           sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl))

(defn var->param [^ParameterizedType type]
  (zipmap (.getTypeParameters (.getRawType type)) (.getActualTypeArguments type)))

(defn specialize-parameterized-type [var->param ^ParameterizedType type]
  (ParameterizedTypeImpl/make
    (.getRawType type)
    (into-array Type (map #(get var->param % %) (.getActualTypeArguments type)))
    (.getOwnerType type)
    ))

(defn class-name [^Type type]
  (condp instance? type
    Class (.getName type)
    ParameterizedType (class-name (.getRawType type))))

(defn specialize-type [var->param ^Type type]
  (if (instance? ParameterizedType type)
    (specialize-parameterized-type var->param type)
    type))

(defn super-types [^Type type]
  (condp instance? type
    Class
    (let [interfaces (seq (.getGenericInterfaces type))
          super (.getGenericSuperclass type)]
      (if (and super (not= Object super)) (cons super interfaces) interfaces))

    ParameterizedType
    (map (partial specialize-type (var->param type)) (super-types (.getRawType type)))

    GenericArrayType nil

    ))

(defn all-types [^Type type] (distinct (tree-seq (partial not= Object) super-types type)))
