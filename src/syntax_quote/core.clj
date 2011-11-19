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
                           (transform-seq env (expand env coll))
                           (if (set? coll)
                             (set (transform-seq env (seq coll)))
                             (if (vector? coll)
                               (vec (transform-seq env (seq coll)))
                               (if (map? coll)
                                 (zipmap
                                  (transform-seq env (keys coll))
                                  (transform-seq env (vals coll)))
                                 coll)))))
        env (env-map)]
    (cons 'do
          ((fn map [fun the-seq]
             (if (seq the-seq)
               (cons (fun (first the-seq))
                     (map fun (rest the-seq)))))
           (partial
            transform
            env)
           body))))

(def ^{:dynamic true} *symbol-table*)

(declare sq syntax-unquote syntax-unquote-splicing)

;; written in a peculiar style because some stuff like 'or' is not
;; defined until after syntax-quote exists

(let-smacros
 ;; setup a nice environment so it feels like full clojure
 {namespace             .getNamespace
  =                     clojure.lang.Util/equiv
  name                  .getName
  >                     clojure.lang.Numbers/gt
  get                   clojure.lang.RT/get
  count                 clojure.lang.RT/count
  dec                   clojure.lang.Numbers/dec
  find-ns               clojure.lang.Namespace/find
  IPersistentCollection clojure.lang.IPersistentCollection
  subs                  .substring
  Var                   clojure.lang.Var
  push-thread-bindings  clojure.lang.Var/pushThreadBindings
  pop-thread-bindings   clojure.lang.Var/popThreadBindings
  map                   (fn m [fun the-seq]
                          (if (seq the-seq)
                            (cons (fun (first the-seq))
                                  (m fun (rest the-seq)))))
  specials              clojure.lang.Compiler/specials
  index-of              .indexOf
  class?                (fn [o] (instance? Class o))}

 (defn ^{:private true} ends-or-contains [haystack-string needle-string]
   (> (index-of haystack-string needle-string) 0))

 (defn ^{:private true} namespace-for [inns sym]
   (let [ns-sym (symbol (namespace sym))
         ns (.lookupAlias inns ns-sym)]
     (if ns ns (find-ns ns-sym))))

 (defn ^{:private true} string-ns [ns]
   (name (name ns)))

 (defn ^{:private true} resolve-symbol [sym]
   (if (ends-or-contains (name sym) ".")
     sym
     (if (namespace sym)
       (let [ns (namespace-for *ns* sym)]
         (if (if (not ns) true (= (string-ns ns) (name sym)))
           sym
           (symbol (string-ns ns) (name sym))))
       (let [o (.getMapping *ns* sym)]
         (if o
           (if (class? o)
             (symbol (name o))
             (if (instance? Var o)
               (symbol (string-ns (.ns o)) (name (.sym o)))))
           (symbol (string-ns *ns*) (name sym)))))))

 (defn ^{:private true} unquote-spliced? [form]
   (if (seq? form)
     (if (symbol? (first form))
       (if (= 'syntax-quote.core/syntax-unquote-splicing
              (resolve-symbol (first form)))
         true))))

 (defn ^{:private true} unquoted? [form]
   (if (seq? form)
     (if (symbol? (first form))
       (if (= 'syntax-quote.core/syntax-unquote
              (resolve-symbol (first form)))
         true))))

  (defn ^{:private true} syntax-quote-seq [the-seq]
   (if (seq the-seq)
     (list 'clojure.core/seq
           (list* 'clojure.core/concat
                  (map
                   (fn [form]
                     (if (unquote-spliced? form)
                       (second form)
                       (if (unquoted? form)
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
     (if (unquoted? form)
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
   (let [sym (get *symbol-table* symbol-name)]
     (if sym
       sym
       (let [sym-stub (subs symbol-name 0 (dec (count symbol-name)))
             new-sym (symbol (gensym sym-stub))]
         (set! *symbol-table* (assoc *symbol-table*
                                symbol-name new-sym))
         new-sym))))

 (defn ^{:private true} special?
   "is this symbol the first of a special form?"
   [sym]
   (if (get specials sym)
     true
     (if (#{'finally '& 'fn* 'catch} sym)
       true)))

 (defn ^{:private true} syntax-quote-symbol-resolve-constructor
   "resolve symbols that are in the form of constructors:
     String. => java.lang.String."
   [symbol-name]
   (let [c (clojure.lang.Compiler/maybeResolveIn
            *ns* (symbol
                  (subs symbol-name 0
                        (dec (count symbol-name)))))]
     (symbol (.concat (name c) "."))))

 (defn ^{:private true} syntax-quote-symbol [sym]
   (list 'quote
         (if (special? sym)
           sym
           (let [symbol-name (name sym)]
             (if (.endsWith symbol-name "#")
               (syntax-quote-gensym-symbol symbol-name)
               (if (if (not (namespace sym))
                     (if (.endsWith symbol-name ".")
                       (not (.startsWith symbol-name "."))))
                 (syntax-quote-symbol-resolve-constructor symbol-name)
                 (if (if (not (namespace sym))
                       (.startsWith symbol-name "."))
                   sym
                   (if (namespace sym)
                     (let [maybe-class
                           (.getMapping *ns* (symbol (namespace sym)))]
                       (if (class? maybe-class)
                         (symbol (name maybe-class) (name sym))
                         (resolve-symbol sym)))
                     (resolve-symbol sym)))))))))

 (defn ^{:private true} sq [form]
   (if (instance? IPersistentCollection form)
     (syntax-quote-collection form)
     (if (symbol? form)
       (syntax-quote-symbol form)
       form)))

 (defn ^{:private true} sq-with-symbol-table [form]
   (push-thread-bindings {#'*symbol-table* {}})
   (try
     (sq form)
     (finally
      (pop-thread-bindings))))

 (defmacro syntax-quote [form]
   (sq-with-symbol-table form))

 )
