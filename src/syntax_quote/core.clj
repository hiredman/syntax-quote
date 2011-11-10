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
   (if-let [ns (namespace form)]
     (if (class? (ns-resolve *ns* (symbol ns)))
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
         (symbol (name (ns-name *ns*)) (name form)))))))

(defmethod recursive-quote ISeq [forms]
  (if (= (first forms) `syntax-unquote)
    (second forms)
    (cons 'list (doall (for [form forms] (syntax-quote-fun form))))))

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
(defmethod syntax-quote-fun ISeq [forms]
  (letfn [(iter [[f & fs]]
            (lazy-seq
             (if (and (instance? ISeq f)
                      (symbol? (first f))
                      (= (ns-resolve *ns* (first f)) #'syntax-unquote-splicing))
               (concat (second f) (when fs (iter fs)))
               (cons (recursive-quote f) (when fs (iter fs))))))]
    (if (and (symbol? (first forms))
             (= #'syntax-unquote (ns-resolve *ns* (first forms))))
      (second forms)
      (cons 'list (doall (iter forms))))))

(defmethod syntax-quote-fun IPersistentSet [forms]
  (set (syntax-quote-fun (seq forms))))

(defmethod syntax-quote-fun IPersistentMap [forms]
  (zipmap (map syntax-quote-fun (keys forms))
          (map syntax-quote-fun (vals forms))))

(defmethod syntax-quote-fun IPersistentVector [forms]
  (list 'clojure.core/vec
        (syntax-quote-fun (seq forms))))

(defn syntax-quote-setup-symbol-table [form]
  (binding [*symbol-table* {}]
    (syntax-quote-fun form)))

(defmacro syntax-quote [form]
  (syntax-quote-setup-symbol-table form))
