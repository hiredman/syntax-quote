(ns syntax-quote.core)

(defmacro env-map []
  (into {} (for [[k _] &env] [(keyword (name k)) k])))

(defmacro let-smacros
  "macros: a map of symbols to what they should be replaced with in the body"
  [macros & body]
  (let [transform (fn [env body]
                    (if (coll? body)
                      ((:transform-coll env) env body)
                      (if (symbol? body)
                        (get macros body body)
                        body)))
        expand (fn [env the-seq]
                 (let [m (macroexpand the-seq)]
                   (if (= m the-seq)
                     m
                     (recur env m))))
        transform-seq (fn [env the-seq]
                        (map (partial transform env) the-seq))
        transform-coll (fn [env coll]
                         (if (seq? coll)
                           ((:transform-seq env) env (expand env coll))
                           (if (set? coll)
                             (set ((:transform-seq env) env (seq coll)))
                             (if (vector? coll)
                               (vec ((:transform-seq env) env (seq coll)))
                               (if (map? coll)
                                 (zipmap
                                  ((:transform-seq env) env (keys coll))
                                  ((:transform-seq env) env (vals coll)))
                                 coll)))))
        env (env-map)]
    (cons 'do
          (map (partial
                transform
                env)
               body))))

(def ^{:dynamic true} *symbol-table*)

(declare sq syntax-unquote syntax-unquote-splicing)

(defn ^{:private true} namespace-for [inns sym]
  (let [ns-sym (symbol (.getNamespace sym))
        ns (.lookupAlias inns ns-sym)]
    (if ns
      ns
      (clojure.lang.Namespace/find ns-sym))))

(defn ^{:private true} resolve-symbol [sym]
  (if (clojure.lang.Numbers/gt (.indexOf (.getName sym) ".") 0)
    sym
    (if (.getNamespace sym)
      (let [ns (namespace-for *ns* sym)]
        (if (if (not ns)
              true
              (clojure.lang.Util/equiv
               (.getName (.getName ns))
               (.getName sym)))
          sym
          (symbol (.getName (.getName ns)) (.getName sym))))
      (let [o (.getMapping *ns* sym)]
        (if o
          (if (instance? Class o)
            (symbol (.getName o))
            (if (instance? clojure.lang.Var o)
              (symbol (.getName (.getName (.ns o)))
                      (.getName (.sym o)))))
          (symbol (.getName (.getName *ns*)) (.getName sym)))))))

(defn ^{:private true} syntax-quote-seq [the-seq]
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
                                'syntax-quote.core/syntax-unquote-splicing
                                (resolve-symbol (first form)))
                             true)))
                     (second form)
                     (if (if (seq? form)
                           (if (symbol? (first form))
                             (if (clojure.lang.Util/equiv
                                  'syntax-quote.core/syntax-unquote
                                  (resolve-symbol (first form)))
                               true)))
                       (list 'clojure.core/list (second form))
                       (list 'clojure.core/list (sq form)))))
                 the-seq)))
    ()))

(defn ^{:private true} syntax-quote-map [the-map]
  (list 'clojure.core/apply
        'clojure.core/hash-map
        (syntax-quote-seq (apply concat the-map))))

(defn ^{:private true} syntax-quote-vector [the-vector]
  (list 'clojure.core/apply
        'clojure.core/vector
        (syntax-quote-seq the-vector)))

(defn ^{:private true} syntax-quote-set [the-vector]
  (list 'clojure.core/set
        (syntax-quote-seq the-vector)))

(defn ^{:private true} syntax-quote-collection [form]
  (if (seq? form)
    (if (if (symbol? (first form))
          (if (clojure.lang.Util/equiv
               'syntax-quote.core/syntax-unquote
               (resolve-symbol (first form)))
            true))
      (second form)
      (syntax-quote-seq form))
    (if (map? form)
      (syntax-quote-map form)
      (if (vector? form)
        (syntax-quote-vector form)
        (if (set? form)
          (syntax-quote-set form)
          form)))))

(defn ^{:private true} syntax-quote-gensym-symbol [symbol-name]
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

(defn ^{:private true} special?
  "is this symbol the first of a special form?"
  [sym]
  (if (clojure.lang.RT/get clojure.lang.Compiler/specials sym)
    true
    (if (#{'finally '& 'fn* 'catch} sym)
      true)))

(defn ^{:private true} syntax-quote-symbol-resolve-constructor
  "resolve symbols that are in the form of constructors:
     String. => java.lang.String."
  [symbol-name]
  (let [c (clojure.lang.Compiler/maybeResolveIn
           *ns* (symbol
                 (.substring symbol-name 0
                             (clojure.lang.Numbers/dec
                              (.length symbol-name)))))]
    (symbol
     (.concat (.getName c) "."))))



(defn ^{:private true} syntax-quote-symbol [form]
  (list 'quote
        (if (special? form)
          form
          (let [symbol-name (.getName form)]
              (if (.endsWith symbol-name "#")
                (syntax-quote-gensym-symbol symbol-name)
                (if (if (not (.getNamespace form))
                      (if (.endsWith symbol-name ".")
                        (not (.startsWith symbol-name "."))))
                  (syntax-quote-symbol-resolve-constructor symbol-name)
                  (if (if (not (.getNamespace form))
                        (.startsWith symbol-name "."))
                    form
                    (if (.getNamespace form)
                      (let [maybe-class (.getMapping *ns*
                                                     (symbol
                                                      (.getNamespace form)))]
                        (if (instance? Class maybe-class)
                          (symbol (.getName maybe-class) (.getName form))
                          (resolve-symbol form)))
                      (resolve-symbol form)))))))))

(defn ^{:private true} sq [form]
  (if (instance? clojure.lang.IPersistentCollection form)
    (syntax-quote-collection form)
    (if (symbol? form)
      (syntax-quote-symbol form)
      form)))

(defn ^{:private true} sq-with-symbol-table [form]
  (clojure.lang.Var/pushThreadBindings {#'*symbol-table* {}})
  (try
    (sq form)
    (finally
     (clojure.lang.Var/popThreadBindings))))

(defmacro syntax-quote [form]
  (let [r (sq-with-symbol-table form)
        id (gensym 'id)]
    (.print System/out id)
    (.print System/out " ")
    (.print System/out form)
    (.print System/out " => ")
    (.print System/out r)
    (list 'do (list '.println 'System/out ''id)
          r)))
