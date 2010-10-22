(ns syntax-quote.core
  (:use [clojure.contrib.java-utils :only [wall-hack-field]]))

(defmulti squote* type)

(defmulti squote*-seq first)

(defmethod squote* :default [form] (list 'quote form))

(defmethod squote* clojure.lang.Symbol [sym]
  (letfn [(generate-symbol [sym] (gensym (name sym)))]
    (list `quote
          (cond
           (namespace sym) sym
           (.endsWith (name sym) "#") (generate-symbol sym)
           :else (symbol (str *ns*) (name sym))))) )

(def splice nil)

(defmethod squote* clojure.lang.ISeq [form] (squote*-seq form))

(defmethod squote*-seq 'unsquote [[_ form]]
  (map
   (fn [el]
     (if-let [[head & tail] (and (coll? el) (= 'squote (first el)) el)]
       (apply squote* tail)
       el))
   form))

(defmethod squote*-seq :default [form]
  (letfn [(reducer [accum form]
                   (let [qf (squote* form)]
                     (if splice
                       (do (set! splice nil)
                           (into accum qf))
                       (conj accum qf))))]
    (binding [splice nil] (cons `list (reduce reducer [] form)))))

(defmethod squote*-seq 'unsquote-splice [form]
  (set! splice true)
  (('unsquote (methods squote*-seq))
   form))

(def symbol-table nil)

(defn squote [form]
  (binding [symbol-table {}]
    (squote* form)))

(defmacro Q [form]
  (squote form))

