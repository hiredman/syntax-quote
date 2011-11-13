(ns syntax-quote.core
  (:import (clojure.lang Symbol ISeq IPersistentSet IPersistentMap
                         IPersistentVector)))

(def ^{:dynamic true} *symbol-table* nil)

(declare syntax-quote-fun)

(defmacro syntax-unquote [form]
  (throw (Exception. (str "Unquote without quote"))))

(defmacro syntax-unquote-splicing [form]
  (throw (Exception. (str "Unquote without quote"))))

(defmulti recursive-quote type)

(defmethod recursive-quote :default [form]
  form)

(defmethod recursive-quote Symbol [form]
  (list
   'quote
   (if (= form 'quote)
     'quote
     (if-let [ns (namespace form)]
       (if (class? (try
                     (ns-resolve *ns* (symbol ns))
                     (catch Exception e)))
         (symbol (.getName (ns-resolve *ns* (symbol ns))) (name form))
         form)
       (let [symbol-name (name form)]
         (if (.endsWith symbol-name "#")
           (or (get *symbol-table* form)
               (let [generated-symbol (gensym
                                       (subs symbol-name 0
                                             (dec (count symbol-name))))]
                 (set! *symbol-table*
                       (assoc *symbol-table* form generated-symbol))
                 generated-symbol))
           (if-let [v (resolve *ns* form)]
             (symbol (name (ns-name (.ns v))) (name (.sym v)))
             (symbol (name (ns-name *ns*)) (name form)))))))))

(defmethod recursive-quote ISeq [forms]
  (cond
   (and (symbol? (first forms))
        (= (resolve (first forms))
           #'syntax-unquote))
   (second forms)
   (and (symbol? (first forms))
        (= (resolve (first forms))
           #'syntax-quote))
   (cons 'syntax-quote.core/syntax-quote
         (doall (for [form (rest forms)] (syntax-quote-fun form))))
   :else
   (cons 'clojure.core/list
         (doall (for [form forms] (syntax-quote-fun form))))))

(defmethod recursive-quote IPersistentSet [forms]
  (set (map syntax-quote-fun forms)))

(defmethod recursive-quote IPersistentMap [forms]
  (zipmap (rest (syntax-quote-fun (keys forms)))
          (rest (syntax-quote-fun (vals forms)))))

(defmethod recursive-quote IPersistentVector [forms]
  (vec (rest (syntax-quote-fun (seq forms)))))

(defmulti syntax-quote-fun type)

(defmethod syntax-quote-fun :default [o] (recursive-quote o))

;; TODO: this doesn't actually unquote
;; TODO: zipper?

#_(defn syntax-quote-a-seq [forms]
    (letfn [(iter [[f & fs]]
              (lazy-seq
               (if (and (instance? ISeq f)
                        (symbol? (first f))
                        (= (ns-resolve *ns* (first f))
                           #'syntax-unquote-splicing))
                 (concat (second f) (when fs (iter fs)))
                 (cons (recursive-quote f) (when fs (iter fs))))))]
      (if (seq forms)
        (doall (iter forms))
        ())))

(defn syntax-quote-a-seq [forms]
  (if (some (fn [form]
              (and (seq? form)
                   (symbol? (first form))
                   (= #'syntax-unquote-splicing
                      (resolve (first form)))))
            forms)
    (cons 'clojure.core/concat
          (map
           (fn [form]
             (if (and (seq? form)
                      (symbol? (first form))
                      (= #'syntax-unquote-splicing
                         (resolve (first form))))
               (second form)
               (list 'clojure.core/list
                     (recursive-quote form))))
           forms))
    (map recursive-quote forms)))

(defmethod syntax-quote-fun ISeq [forms]
  (let [[op & args] forms]
    (cond
     (and (symbol? op)
          (= (resolve op)
             #'syntax-quote))
     (doall (list* 'syntax-quote.core/syntax-quote args))
     (and (symbol? op)
          (= (resolve op)
             #'syntax-unquote))
     (first args)
     :else
     (cons 'clojure.core/list (doall (syntax-quote-a-seq forms))))))

(defmethod syntax-quote-fun IPersistentSet [forms]
  (set (syntax-quote-fun (seq forms))))

(defmethod syntax-quote-fun IPersistentMap [forms]
  (list 'clojure.core/apply 'clojure.core/hash-map
        (cons 'clojure.core/list
              (doall (syntax-quote-a-seq (mapcat identity forms))))))

(defmethod syntax-quote-fun IPersistentVector [forms]
  (list 'clojure.core/vec
        (doall (syntax-quote-fun (seq forms)))))

(defn syntax-quote-setup-symbol-table [form]
  (binding [*symbol-table* {}]
    (syntax-quote-fun form)))

(defmacro syntax-quote [form]
  (syntax-quote-setup-symbol-table form))
