(ns parsing-with-derivatives.core
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [parsing-with-derivatives.protocols :refer :all]
            [parsing-with-derivatives.fixpoint :refer :all]
            [clojure.set :as s]
            [rhizome.viz :refer :all]
            [rhizome.dot :as dot]
            [clojure.string :as string]
            [parsing-with-derivatives.graph :as graph]
            [parsing-with-derivatives.walk2 :as walk])
  (:import [clojure.lang Keyword]))

(defn make-parse-tree [x y]
  (let [merge-x (:merge (meta x))
        merge-y (:merge (meta y))]
    (cond
     (= ::no-result x) (if merge-y y [y])
     (= ::no-result y) (if merge-x x [x])
     (and merge-x merge-y) (concat x y)
     merge-x (concat x [y])
     merge-y (cons x y)
     :default [x y])))

(defn parser?
  "Parser type check; useful in assertions, for helping core.typed."
  [x]
  (extends? Parser (class x)))

;; parser constructors
(declare empty eps eps* red)
(declare ->Cat ->Alt ->Delta ->Star ->Red)

(defn cat
  ([p1 p2] (->Cat p1 p2))
  ([p1 p2 & ps]
  (cat p1 (reduce #(->Cat %1 %2)
                  p2
                  ps))))

(defn alt [p1 p2] (->Alt p1 p2))
(defn delta [p1] (->Delta p1))
(defn star [p1] (->Star p1))
(defn plus [p1] (->Cat p1 (->Star p1)))
(defn red [p1 f] (->Red p1 f))

;; delayed parser constructors
(defmacro cat' [p1 p2] `(->Cat (delay ~p1) (delay ~p2)))
(defmacro alt' [p1 p2] `(->Alt (delay ~p1) (delay ~p2)))
(defmacro delta' [p1] `(->Delta (delay ~p1)))
(defmacro star' [p1] `(->Star (delay ~p1)))
(defmacro red' [p1 f] `(->Red (delay ~p1) ~f))

(defn graph-label [this]
  (-graph-label this))

  (defn children [this]
  (-children this))

(def parse-null
  (fix-memo '()
    (fn [p] (-parse-null p))))

(def derivative
  (memoize
   (fn [p c] (-derivative p c))))

(def is-null?
  (fix-memo true
    (fn [p] (-is-null? p))))


(def is-empty?
  (fix-memo true
    (fn [p] (-is-empty? p))))

(def compact
  (memoize
   (fn [p]
     (cond
      (is-empty? p) empty
      (is-null? p) (eps* (parse-null p))
      :default (-compact p)))))


(defrecord Empty []
  Parser
  (-graph-label [_] "∅")
  (-children [_] [])
  (-parse-null [_] '())
  (-derivative [_ c] empty)
  (-is-empty? [_] true)
  (-is-null? [_] false)
  (-compact [this] this))

(def empty (->Empty))


(defrecord Epsilon [val]
  Parser
  (-graph-label [_]
    (if (= val [::no-result])
      "ε"
      ["ε" (graph/escape-string (pr-str val))]))
  (-children [_] #{})
  (-parse-null [_] val)
  (-derivative [_ c] empty)
  (-is-empty? [_] false)
  (-is-null? [_] true)
  (-compact [this] this))

(def eps (->Epsilon '(::no-result)))
(defn eps* [trees] (->Epsilon trees))

(extend-protocol Parser
  java.lang.Character
  (-graph-label [this] (str \" this \"))
  (-children [_] [])
  (-parse-null [_] [])
  (-derivative [this c] (if (= this c) (eps* (list c)) empty))
  (-is-empty? [_] false)
  (-is-null? [_] false)
  (-compact [this] this))

(defmacro proxy-parser [type graph-label-fn proxy-fn]
  `(extend-protocol Parser
     ~type
     (-graph-label [this#] (~graph-label-fn this#))
     (-children [this#] [(~proxy-fn this#)])
     (-parse-null [this#] (parse-null (~proxy-fn this#)))
     (-derivative [this# c#] (derivative (~proxy-fn this#) c#))
     (-is-empty? [this#] (is-empty? (~proxy-fn this#)))
     (-is-null? [this#] (is-null? (~proxy-fn this#)))
     (-compact [this#] (compact (~proxy-fn this#)))
))

(defn deref-parser [r]
  (let [p (deref r)]
    (assert (parser? p) (str p " is not a parser"))
    p))

(proxy-parser clojure.lang.Atom  (fn [a] (str (:rule-name (meta a)))) deref-parser)
(proxy-parser clojure.lang.Delay (fn [_] "~") deref-parser)

(defrecord Delta [p1]
  Parser
  (-graph-label [_] "δ")
  (-children [_] [p1])
  (-parse-null [_] (parse-null p1))
  (-derivative [_ c] empty)
  (-is-empty? [_] (empty-coll? (parse-null p1)))
  (-is-null? [_] (is-null? p1))
  (-compact [this]
    (let [r1 (parse-null p1)]
      (if (empty-coll? r1)
        empty
        (eps* r1)))))

(defn is-null-singleton? [p]
  (and (is-null? p) (= 1 (count (parse-null p)))))

(defrecord Cat [p1 p2]
  Parser
  (-graph-label [_] "cat")
  (-children [_] [p1 p2])
  (-parse-null [_]
    (for [t1 (parse-null p1), t2 (parse-null p2)]
      (with-meta (make-parse-tree t1 t2) {:merge true})))

  (-derivative [_ c] (alt (cat' (delta p1) (derivative p2 c))
                          (cat' (derivative p1 c) p2)))

  (-is-empty? [_] (or (is-empty? p1) (is-empty? p2)))
  (-is-null? [_] (and (is-null? p1) (is-null? p2)))
  (-compact [this]
    (cond
     ;; the below rule is bogus!
     ;; (and (is-null? p1) (is-null? p2)) (parse-null this)

     (is-null-singleton? p1)
     (red (compact p2)
          #(with-meta (make-parse-tree (first (parse-null p1)) %)
                      {:merge true}))

     (is-null-singleton? p2)
     (red (compact p1)
          #(with-meta (make-parse-tree % (first (parse-null p2)))
                      {:merge true}))
     :default (cat' (compact p1) (compact p2))))
)

(defrecord Alt [p1 p2]
  Parser
  (-graph-label [_] "alt")
  (-children [_] [p1 p2])
  (-parse-null [_] (distinct (concat (parse-null p1) (parse-null p2))))
  (-derivative [_ c] (alt' (derivative p1 c) (derivative p2 c)))
  (-is-empty? [_] (and (is-empty? p1) (is-empty? p2)))
  (-is-null? [_] (and (is-null? p1) (is-null? p2)))
  (-compact [this]
    (cond
     (is-empty? p1) (compact p2)
     (is-empty? p2) (compact p1)
     :default (alt' (compact p1) (compact p2))))
)

(defrecord Star [p1]
  Parser
  (-graph-label [_] "*")
  (-children [_] #{p1})
  (-parse-null [_] [::no-result])
  (-derivative [this c] (cat' (derivative p1 c) this))
  (-is-empty? [_] false)
  (-is-null? [_] (or (is-null? p1) (is-empty? p1)))
  (-compact [this] (star' (compact p1)))
)

(defrecord Red [p1 f]
  Parser
  (-graph-label [_] [(str "→"
                          (try (graph/escape-string (pr-str (f \%)))
                               (catch Exception e "_")))])
  (-children [_] [p1])
  (-parse-null [_] (with-meta (map f (parse-null p1))
                              {:merge true}))
  (-derivative [_ c] (red (derivative p1 c) f))
  (-is-empty? [_] (is-empty? p1))
  (-is-null? [_] (is-null? p1))
  (-compact [this]
    (cond
     ;; (and (instance? Cat p1) (is-null? (:p1 p1)))
     ;; (red (compact (:p2 p1)) op arg1)

     (instance? Red p1)
     (red (compact (:p1 p1)) (comp f (:f p1)))

     :default (red (compact p1) f)))
)

(defrecord Hide [p1]
  Parser
  (-graph-label [_] "hide")
  (-children [_] [p1])
  (-parse-null [_] (if (is-null? p1) [::no-result] (parse-null p1)))
  (-derivative [_ c] (->Hide (derivative p1 c)))
  (-is-empty? [_] (is-empty? p1))
  (-is-null? [_] (is-null? p1))
  (-compact [this] (->Hide (compact p1)))
)

(defn hide [p1] (->Hide p1))

(defn graph-size [p]
  (count (graph/bfs children p)))

(defn view-parser-graph [p]
  (view-graph
   (graph/bfs children p) children
   :node->descriptor (fn [n] {:label (graph-label n)})))


(defn grammar-vec? [x]
  (and (vector? x) (keyword? (first x))))

(defn label [tag parser]
  (red parser (fn [parse-tree]
                (if (= parse-tree ::no-result)
                  ::no-result
                  (make-parse-tree tag parse-tree)))))

(defn drop-last-kw-char [kw]
  (keyword (apply str (drop-last (name kw)))))

(defn is-hidden-rule? [rule]
  (= \- (last (name rule))))

(defn normalize-rule [rule]
  (if (is-hidden-rule? rule)
    (drop-last-kw-char rule)
    rule))

(defn grammar->parser
  "Turn a grammar using keywords for rules into a parser. Each rule
   gets an atom, allowing for recursion. "
  ([grammar-vec]
     (grammar->parser (apply hash-map grammar-vec) (first grammar-vec)))
  ([grammar start-rule]
     (let [atomized-grammar
           (into {} (for [[rule production] grammar]
                      (if (is-hidden-rule? rule)
                        (let [rule (drop-last-kw-char rule)]
                          [rule (atom production
                                      :meta {:rule-name rule})])
                        [rule (atom (label rule production)
                                    :meta {:rule-name rule})])))
           start-rule (normalize-rule start-rule)]

       (doseq [[rule production-atom] atomized-grammar]
         (swap! production-atom #(walk/postwalk-replace atomized-grammar %)))
       (atomized-grammar start-rule))))

(defn coerce-parser [grammar-or-parser]
  (if (grammar-vec? grammar-or-parser)
    (grammar->parser grammar-or-parser)
    grammar-or-parser))


(defn full-derivative [parser input]
  ;; (view-parser-graph parser)
  (if-let [c (first input)]
    (let [d (derivative parser c)
          d' (compact d)]
      (recur d' (rest input)))
    parser))


(defn parse
  ([parser input]
     (let [parser (coerce-parser parser)]
       (parse-null (full-derivative parser input))))
  ([g p input]
     (let [p (grammar->parser g p)]
       (parse p input))))
