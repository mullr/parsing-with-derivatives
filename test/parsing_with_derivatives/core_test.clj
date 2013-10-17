(ns parsing-with-derivatives.core-test
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [clojure.test :refer :all]
            [parsing-with-derivatives.core :refer :all]
            [clojure.core.typed :refer [check-ns]]))

(deftest parsing
  (are [parser str first-ast] (= [first-ast] (parse parser str))
    \a "a" \a

    (cat \a \b)          "ab"  [\a \b]
    (cat \a (cat \b \c)) "abc" [\a [\b \c]]
    (cat (cat \a \b) \c) "abc" [[\a \b] \c]

    (star \a) "aaa" [\a [\a [\a]]]

    ;; Atom as parser
    (atom \a) "a" \a

    ;; grammar
    [:S \a] "a" \a
    [:S (cat \a \b)] "ab" [\a \b]

    ;; recursion
    [:S (alt eps (cat :S \a))] "aa" [[\a] \a]
    [:S (alt eps (cat \a :S))] "aa" [\a [\a]])

  ;; Reducers
  (is (= [1] (parse (red \1 #(Integer/parseInt (str %))) "1"))))

(defn graph-size-with [parser input]
  (graph-size (full-derivative parser input)))

(deftest graph-growth
  (is (> 15 (graph-size-with left-recursive-as (repeat 25 \a))))
  (is (> 15 (graph-size-with right-recursive-as (repeat 25 \a)))))

(def expression
  {:digit (reduce alt [\1 \2 \3 \4 \5 \6 \7 \8 \9 \0])
   :number (red (plus :digit) #(Integer/parseInt (apply str %)))
   :value (alt :number (cat \( :expr \)))
   :mult-op (alt \* \/)
   :mult-expr (alt :value (cat :value :mult-op :value))
   :add-op (alt \+ \-)
   :add-expr (alt :mult-expr (cat :mult-expr :add-op :mult-expr))
   :expr :add-expr
   })

(deftest expression-test
  (are [str first-ast] (= [first-ast] (parse expression :expr str))
    "1"     1
    "12"    12
    "12*13" [12 \* 13]
    "12+13" [12 \+ 13]
    "1*2+3" [[1 \* 2] \+ 3]
    "1*(2+3)" [1 \* [\( [2 \+ 3] \)]]
))
