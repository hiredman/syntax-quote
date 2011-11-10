(ns syntax-quote.core-test
  (:use [syntax-quote.core]
        [clojure.test]))

(deftest t-syntax-quote
  (are [x y] (= x y)
       `(1 2 3)            (syntax-quote (1 2 3))
       `foo                (syntax-quote foo)
       `{:a 1}             (syntax-quote {:a 1})
       `{:a `{:b `(1 2 `[3])}} (syntax-quote
                                {:a (syntax-quote
                                     {:b (syntax-quote
                                          (1 2 (syntax-quote [3])))})})
       `Boolean/TYPE       (syntax-quote Boolean/TYPE)
       `foo/bar            (syntax-quote foo/bar)
       `[foo bar baz]      (syntax-quote [foo bar baz])
       `~+                 (syntax-quote (syntax-unquote +))
       [1 2 3 4]           (syntax-quote
                            [1 2 (syntax-unquote-splicing
                                  [3 4])])
       `{:a [1 2 ~@[3 4]]} (syntax-quote
                            {:a [1 2 (syntax-unquote-splicing
                                      [3 4])]}))
  (is (not (= 'foo (syntax-quote foo#))))
  (let [[f1 f2] (syntax-quote [foo# foo#])]
    (is (= f1 f2))))
