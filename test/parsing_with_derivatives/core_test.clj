(ns parsing-with-derivatives.core-test
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [clojure.test :refer :all]
            [parsing-with-derivatives.core :refer :all]
            [clojure.core.typed :refer [check-ns]]))


(deftest typecheck
  (is (check-ns 'parsing-with-derivatives.core)))

(deftest parsing
  (is (= [\a] (parse \a "a")))
  (is (= [[\a \b]] (parse (cat \a \b) "ab")))
  (is (= [[\a \b]] (parse (cat \a \b) "ab")))
  (is (= [[\a [\b \c]]] (parse (cat \a (cat \b \c)) "abc")))
  (is (= [[[\a \b] \c]] (parse (cat (cat \a \b) \c) "abc")))
  
  ;; Atom as parser
  (is (= [\a] (parse (atom \a) "a")))
 
  ;; grammar
  (is (= [\a] (parse {:S \a} :S "a")))
  (is (= [[\a \b]] (parse {:S (cat \a \b)} :S "ab")))

  ;; ;; left-recursive
  (is (= [[[nil \a] \a]] 
         (parse {:S (alt eps (cat :S \a))} :S "aa")))

  ;; right-recursive
  (is (= [[\a [\a nil]]] 
         (parse {:S (alt eps (cat \a :S))} :S "aa")))

)


