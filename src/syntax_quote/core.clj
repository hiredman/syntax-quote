(ns syntax-quote.core)

(def ^{:dynamic true} *symbol-table*)

(declare sq syntax-unquote syntax-unquote-splicing)

(defn syntax-quote-seq [the-seq]
  (if (seq the-seq)
    (list 'clojure.core/seq
          (list* 'clojure.core/concat
                ((fn m [fun the-seq]
                   (if (seq the-seq)
                     (cons (fun (first the-seq))
                           (m fun (rest the-seq)))))
                 (fn [form]
                   (if (if (seq? form)
                         (if (symbol? (first form))
                           (if (clojure.lang.Util/equiv
                                #'syntax-unquote-splicing
                                (ns-resolve *ns* (first form)))
                             true)))
                     (second form)
                     (if (if (seq? form)
                           (if (symbol? (first form))
                             (if (clojure.lang.Util/equiv
                                #'syntax-unquote
                                (ns-resolve *ns* (first form)))
                               true)))
                       (list 'clojure.core/list (second form))
                       (list 'clojure.core/list (sq form)))))
                 the-seq)))
    ()))

(defn syntax-quote-map [the-map]
  (list 'clojure.core/apply
        'clojure.core/hash-map
        (syntax-quote-seq (apply concat the-map))))

(defn syntax-quote-vector [the-vector]
  (list 'clojure.core/apply
        'clojure.core/vector
        (syntax-quote-seq the-vector)))

(defn syntax-quote-collection [form]
  (if (seq? form)
    (if (if (symbol? (first form))
          (if (clojure.lang.Util/equiv
               #'syntax-unquote
               (ns-resolve *ns* (first form)))
            true))
      (second form)
      (syntax-quote-seq form))
    (if (map? form)
      (syntax-quote-map form)
      (if (vector? form)
        (syntax-quote-vector form)))))

(defn syntax-quote-gensym-symbol [symbol-name]
  (let [sym (clojure.lang.RT/get *symbol-table* symbol-name)]
    (if sym
     sym
     (let [new-sym (symbol (gensym (.substring symbol-name 0
                                               (clojure.lang.Numbers/dec
                                                (clojure.lang.RT/count
                                                 symbol-name)))))]
       (set! *symbol-table* (assoc *symbol-table*
                              symbol-name new-sym))
       new-sym))))


(defn syntax-quote-symbol [form]
  (list 'quote
        (if (if (clojure.lang.RT/get clojure.lang.Compiler/specials form)
              true
              (if (#{'finally '& 'fn* 'catch} form)
                true))
          form
          (if (.getNamespace form)
            (let [the-class (try
                              (clojure.lang.Compiler/maybeResolveIn
                               *ns* (symbol (.getNamespace form)))
                              (catch Exception _))]
              (if (instance? Class the-class)
                (symbol (.getName the-class) (.getName form))
                (let [v (try
                          (clojure.lang.Compiler/maybeResolveIn *ns* form)
                          (catch Exception _))]
                  (if v
                    (symbol (.getName (.getName (.ns v))) (.getName form))
                    form))))
            (let [symbol-name (.getName form)]
              (if (.endsWith symbol-name "#")
                (syntax-quote-gensym-symbol symbol-name)
                (let [idx (.indexOf symbol-name ".")]
                  (if (if (clojure.lang.Numbers/gt idx 1)
                        (if (clojure.lang.Numbers/lt
                             idx
                             (clojure.lang.RT/count symbol-name))
                          true))
                    form
                    (let [v (try
                              (clojure.lang.Compiler/maybeResolveIn *ns* form)
                              (catch Exception _))]
                      (if v
                        (if (instance? Class v)
                          (symbol (.getName v))
                          (symbol (.getName (.getName (.ns v)))
                                  (.getName (.sym v))))
                        (symbol (.getName (.getName *ns*))
                                symbol-name)))))))))))

(defn sq [form]
  (if (instance? clojure.lang.IPersistentCollection form)
    (syntax-quote-collection form)
    (if (symbol? form)
      (syntax-quote-symbol form)
      form)))

(defn sq-with-symbol-table [form]
  (clojure.lang.Var/pushThreadBindings {#'*symbol-table* {}})
  (try
    (sq form)
    (finally
     (clojure.lang.Var/popThreadBindings))))

(defmacro syntax-quote [form]
  (let [r (sq-with-symbol-table form)]
    (.print System/out form)
    (.print System/out " => ")
    (.print System/out r)
    r))
