(ns syntax-quote.core-test
  (:use [syntax-quote.core]
        [clojure.test]))

(def x 1)
(def y 2)

(deftest t-syntax-quote
  (is (= `(1 2 3) (syntax-quote (1 2 3))))
  (is (= `foo (syntax-quote foo)))
  (is (= `{:a 1} (syntax-quote {:a 1})))
  (is (= `Boolean/TYPE (syntax-quote Boolean/TYPE)))
  (is (= `foo/bar (syntax-quote foo/bar)))
  (is (= `{a b} (syntax-quote {a b})))
  (is (= `[foo bar baz] (syntax-quote [foo bar baz])))
  (is (= `~+ (syntax-quote (syntax-unquote +))))
  (is (= [1 2 3 4]
         (syntax-quote [1 2 (syntax-unquote-splicing [3 4])])))
  (is (= `{:a [1 2 ~@[3 4]]}
         (syntax-quote
          {:a [1 2 (syntax-unquote-splicing [3 4])]})))
  (is (= `() (syntax-quote ())))
  (is (not (= 'foo (syntax-quote foo#))))
  (let [[f1 f2] (syntax-quote [foo# foo#])]
    (is (= f1 f2)))
  (let [{rdr :a} (macroexpand-1 `{:a `{:b `(1 2 `[a])}})
        {mac :a} (macroexpand-1
                  (syntax-quote
                   {:a (syntax-quote
                        {:b (syntax-quote (1 2 (syntax-quote [a])))})}))
        {rdr :b} (eval rdr)
        {mac :b} (eval mac)
        [x1 y1 rdr] (eval rdr)
        [x2 y2 mac] (eval mac)]
    (is (= x1 x2))
    (is (= y1 y2))
    (is (= (eval rdr)
           (eval mac))))
  (is (= `(. clojure.lang.Util equiv ~x ~y)
         (syntax-quote (. clojure.lang.Util equiv
                          (syntax-unquote x)
                          (syntax-unquote y)))))
  (is (= `(. clojure.lang.Util identical ~x ~y)
         (syntax-quote (. clojure.lang.Util identical
                          (syntax-unquote x)
                          (syntax-unquote y)))))
  (is (= `& (syntax-quote &)))

  (is (= `'foo (syntax-quote (quote foo))))
  
  (is (= `(binding [*ns* *ns*]
            (clojure.core/use
             (quote clojure.core))
            (eval (quote
                   (do
                     ~@[1 2 3]))))
         (syntax-quote
          (binding [*ns* *ns*]
            (clojure.core/use
             (quote clojure.core))
            (eval (quote
                   (do
                     (syntax-unquote-splicing
                      [1 2 3]))))))))
  (is (= `'clojure.core
         (syntax-quote 'clojure.core)))
  (is (= `'foo.bar (syntax-quote 'foo.bar)))
  (is (= `.. (syntax-quote ..)))
  (is (= `(Object.) (syntax-quote (Object.))))
  (is (= `(reify* (clojure.core/unquote 'interfaces)
                  (clojure.core/unquote-splicing ['m1 'm2]))
         (syntax-quote
          (reify* (syntax-unquote 'interfaces)
                  (syntax-unquote-splicing ['m1 'm2])))))
  (is (= `#{1 2} (syntax-quote #{1 2})))
  (is (= `#{~:foo} (syntax-quote #{(syntax-unquote :foo)}))))
