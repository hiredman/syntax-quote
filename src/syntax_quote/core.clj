(ns syntax-quote.core
  (:import (clojure.lang Symbol ISeq IPersistentSet IPersistentMap
                         IPersistentVector)))

(def ^{:dynamic true} *symbol-table*)

(declare sq syntax-unquote syntax-unquote-splicing)

(defn syntax-quote-seq [the-seq]
  (if-not (empty? the-seq)
    (->> (for [form the-seq]
           (if (and (seq? form)
                    (symbol? (first form))
                    (= #'syntax-unquote-splicing
                       (ns-resolve *ns* (first form))))
             (second form)
             (list 'clojure.core/list (sq form))))
         doall
         (cons 'clojure.core/list)
         (list 'clojure.core/apply
               'clojure.core/concat)
         (list 'clojure.core/seq))
    ()))

(defn syntax-quote-map [the-map]
  (->> (apply concat the-map)
       syntax-quote-seq
       (list 'clojure.core/apply
             'clojure.core/hash-map)))

(defn syntax-quote-vector [the-vector]
  (->> the-vector
       syntax-quote-seq
       (list 'clojure.core/apply
             'clojure.core/vector)))

(defn syntax-quote-collection [form]
  (if (seq? form)
    (if (and (symbol? (first form))
             (= #'syntax-unquote
                (ns-resolve *ns* (first form))))
      (second form)
      (syntax-quote-seq form))
    (if (map? form)
      (syntax-quote-map form)
      (if (vector? form)
        (syntax-quote-vector form)))))

(defn syntax-quote-gensym-symbol [symbol-name]
  (if-let [sym (get *symbol-table* symbol-name)]
    sym
    (let [new-sym (symbol (gensym (subs symbol-name 0
                                        (dec (count symbol-name)))))]
      (set! *symbol-table* (assoc *symbol-table*
                             symbol-name new-sym))
      new-sym)))

(defn syntax-quote-symbol [form]
  (->> (if-not (namespace form)
         (let [symbol-name (name form)]
           (if (.endsWith symbol-name "#")
             (syntax-quote-gensym-symbol symbol-name)
             (if-let [v (ns-resolve *ns* form)]
               (symbol (name (ns-name (.ns v))) (name (.sym v)))
               (symbol (name (ns-name *ns*)) symbol-name))))
         (if-let [the-class (try
                              (ns-resolve *ns* (symbol (namespace form)))
                              (catch Exception _))]
           (symbol (.getName the-class) (name form))
           form))
       (list 'quote)))

(defn sq [form]
  (if (coll? form)
    (syntax-quote-collection form)
    (if (symbol? form)
      (syntax-quote-symbol form)
      form)))

(defn sq-with-symbol-table [form]
  (binding [*symbol-table* {}]
    (sq form)))

(defmacro syntax-quote [form]
  (sq-with-symbol-table form))
