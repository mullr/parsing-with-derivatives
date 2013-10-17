(ns parsing-with-derivatives.core-test
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [clojure.test :refer :all]
            [parsing-with-derivatives.core :refer :all]
            [clojure.core.typed :refer [check-ns]]))


(deftest typecheck
  (is (check-ns 'parsing-with-derivatives.core)))

(def left-recursive-as  (grammar->parser {:S (alt eps (cat :S \a))} :S))
(def right-recursive-as (grammar->parser {:S (alt eps (cat \a :S))} :S))

(defn graph-size-with [parser input]
  (graph-size (full-derivative parser input)))

(deftest parsing
  (is (= [\a] (parse \a "a")))
  (is (= [[\a \b]] (parse (cat \a \b) "ab")))
  (is (= [[\a \b]] (parse (cat \a \b) "ab")))
  (is (= [[\a [\b \c]]] (parse (cat \a (cat \b \c)) "abc")))
  (is (= [[[\a \b] \c]] (parse (cat (cat \a \b) \c) "abc")))
  (is (= [[\a [\a [\a nil]]]  (parse (star \a) "aaa")]))

  ;; Reducers
  (is (= [1] (parse (red \1 #(Integer/parseInt (str %))) "1")))

  ;; Atom as parser
  (is (= [\a] (parse (atom \a) "a")))
 
  ;; grammar
  (is (= [\a] (parse {:S \a} :S "a")))
  (is (= [[\a \b]] (parse {:S (cat \a \b)} :S "ab")))

  ;; recursion
  (is (= [[[nil \a] \a]] (parse left-recursive-as  "aa")))
  (is (= [[\a [\a nil]]] (parse right-recursive-as "aa")))

  (is (> 15 (graph-size-with left-recursive-as (repeat 25 \a))))
  (is (> 15 (graph-size-with right-recursive-as (repeat 25 \a))))
)

(def expression
  {:digit (reduce alt [\1 \2 \3 \4 \5 \6 \7 \8 \9 \0])
   :number (red (plus :digit) #(Integer/parseInt (apply str %)))
   :value (alt :number (cat \( :expr \)))
   :mult-op (alt \* \/)
   :mult-expr (alt :value (cat :number :mult-op :number))
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
))
