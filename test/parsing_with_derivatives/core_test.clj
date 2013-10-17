(ns parsing-with-derivatives.core-test
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [clojure.test :refer :all]
            [parsing-with-derivatives.core :refer :all]
            [clojure.core.typed :refer [check-ns]]))

(deftest parsing
  (are [parser str first-ast] (= [first-ast] (parse parser str))
    \a "a" \a

    [:S  \a] "a" [:S \a]
    [:S- \a] "a" \a
    [:S  (cat \a \b)] "ab" [:S \a \b]
    [:S- (cat \a \b)] "ab" [\a \b]

    [:S  (cat \a :T)
     :T  (cat \b \c)]     "abc" [:S \a [:T \b \c]]

    [:S  (cat :T \c)
     :T  (cat \a \b)]     "abc" [:S [:T \a \b] \c]

    [:S  (cat :T \c)
     :T- (cat \a \b)]     "abc" [:S \a \b \c]

    [:S  (star \a)]       "aaa" [:S \a \a \a]
    [:S  (plus \a)]       "aaa" [:S \a \a \a]

    ;; Atom as parser
    (atom \a) "a" \a

    ;; recursion
    [:S (alt eps (cat :S \a))] "aa" [:S [:S \a] \a]
    [:S (alt eps (cat \a :S))] "aa" [:S \a [:S \a]])

  ;; Reducers
  (is (= [1] (parse (red \1 #(Integer/parseInt (str %))) "1"))))

(defn graph-size-with [grammar input]
  (graph-size (full-derivative (grammar->parser grammar) input)))

(deftest graph-growth
  (is (> 15 (graph-size-with [:S (alt eps (cat :S \a))] (repeat 25 \a))))
  (is (> 15 (graph-size-with [:S (alt eps (cat \a :S))] (repeat 25 \a))))
  )

(def expression
  {:digit- (reduce alt [\1 \2 \3 \4 \5 \6 \7 \8 \9 \0])
   :number- (red (plus :digit) #(Integer/parseInt (apply str %)))
   :value- (alt :number
                (cat \( :add-expr \)))
   :mult-op- (alt \* \/)
   :mult-expr- (alt :value
                    (label :mult (cat :value :mult-op :value)))
   :add-op- (alt \+ \-)
   :add-expr- (alt :mult-expr
                   (label :add (cat :mult-expr :add-op :mult-expr)))
   :expr :add-expr})

(deftest expression-test
  (are [str first-ast] (= [first-ast] (parse expression :expr str))
    "1"       [:expr 1]
    "12"      [:expr 12]
    "12*13"   [:expr [:mult 12 \* 13]]
    "12+13"   [:expr [:add 12 \+ 13]]
    "1*2+3"   [:expr [:add [:mult 1 \* 2] \+ 3]]
    "1*(2+3)" [:expr [:mult 1 \* \( [:add 2 \+ 3] \)]]
))
