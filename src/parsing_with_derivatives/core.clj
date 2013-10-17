(ns parsing-with-derivatives.core
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [clojure.core.typed :refer :all]
            [parsing-with-derivatives.protocols :refer :all]
            [parsing-with-derivatives.fixpoint :refer :all]
            [clojure.set :as s]
            [rhizome.viz :refer :all]
            [rhizome.dot :as dot]
            [clojure.string :as string]
            [parsing-with-derivatives.graph :as graph]
            [parsing-with-derivatives.walk2 :as walk])
  (:import [clojure.lang Keyword]))

;; Type annotations for clojure.core functions
(ann ^:no-check clojure.core/distinct
     (All [x] [(Seq x) -> (Seq x)]))
(ann ^:no-check clojure.core/partition-by 
     (All [x y] [[x -> y] (Seqable x) -> (Seq x)]))
(ann ^:no-check clojure.core/drop-last
     (All [x] [(Seqable x) -> (Seq x)]))

;; The with-meta annotation that comes with core.typed doesn't work
;; with vectors. Use a looser one here.
(ann ^:no-check clojure.core/with-meta (All [x] [x (Map Keyword Any) -> x]))

;; Annotate internal libraries
(ann ^:no-check parsing-with-derivatives.fixpoint/fix-memo
     (All [x y] [y [x -> y] -> [x -> y]]))

(ann ^:no-check make-parse-tree [ParseTree ParseTree -> ParseTree])
(defn make-parse-tree "Parse tree constructor" [x y]
  (cond
   (= ::no-result x) [y]
   (= ::no-result y) [x]
   (and (:merge (meta x)) (:merge (meta y))) (concat x y)
   (:merge (meta x)) (concat x [y])
   (:merge (meta y)) (cons x y)
   :default [x y]))

(ann ^:no-check parser?
     [Any -> Boolean :filters {:then (is Parser 0), :else (! Parser 0)}])
(defn parser?
  "Parser type check; useful in assertions, for helping core.typed."
  [x]
  (extends? Parser (class x)))

;; parser constructors
(declare empty eps eps* red)
(declare ->Cat ->Alt ->Delta ->Star ->Red)

(ann cat (Fn [Parser Parser -> Cat]
             [Parser Parser Parser * -> Cat]))
(defn cat
  ([p1 p2] (->Cat p1 p2))
  ([p1 p2 & ps]
  (cat p1 (reduce (ann-form #(->Cat %1 %2)
                            [Parser Parser -> Cat])
                  p2
                  ps))))

(ann alt [Parser Parser -> Alt])
(defn alt [p1 p2] (->Alt p1 p2))

(ann delta [Parser -> Delta])
(defn delta [p1] (->Delta p1))

(ann star [Parser -> Star])
(defn star [p1] (->Star p1))

(ann plus [Parser -> Parser])
(defn plus [p1] (->Cat p1 (->Star p1)))

(ann red [Parser ReducerFn -> Red])
(defn red [p1 f] (->Red p1 f))

;; delayed parser constructors
(defmacro cat' [p1 p2] `(->Cat (delay ~p1) (delay ~p2)))
(defmacro alt' [p1 p2] `(->Alt (delay ~p1) (delay ~p2)))
(defmacro delta' [p1] `(->Delta (delay ~p1)))
(defmacro star' [p1] `(->Star (delay ~p1)))
(defmacro red' [p1 f] `(->Red (delay ~p1) ~f))

(ann graph-label [Parser -> (U String (Vec String))])
(defn graph-label [this]
  (-graph-label this))

(ann children [Parser -> (Seqable Parser)])
(defn children [this]
  (-children this))

(ann parse-null [Parser -> ParseForest])
(def parse-null
  (fix-memo '() 
    (fn> [p :- Parser] (-parse-null p))))

(ann derivative [Parser Token -> Parser])
(def derivative
  (memoize
   (fn> [p :- Parser, c :- Token] (-derivative p c))))

(ann is-null? [Parser -> Boolean])
(def is-null? 
  (fix-memo true
    (fn> [p :- Parser] (-is-null? p))))

(ann is-empty? [Parser -> Boolean])
(def is-empty? 
  (fix-memo true
    (fn> [p :- Parser] (-is-empty? p))))

(ann compact [Parser -> Parser])
(def compact 
  (memoize
   (fn> [p :- Parser]
     (cond
      (is-empty? p) empty
      (is-null? p) (eps* (parse-null p))
      :default (-compact p)))))


(ann-record Empty [])
(defrecord Empty []
  Parser
  (-graph-label [_] "∅")
  (-children [_] [])
  (-parse-null [_] '())
  (-derivative [_ c] empty)
  (-is-empty? [_] true)
  (-is-null? [_] false)
  (-compact [this] this))

(ann empty Empty)
(def empty (->Empty))


(ann-record Epsilon [val :- ParseForest])
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

(ann eps Epsilon)
(def eps (->Epsilon '(::no-result)))

(ann eps* [ParseForest -> Epsilon])
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

(ann deref-parser [(clojure.lang.IDeref Any) -> Parser])
(defn deref-parser [r]
  (let [p (deref r)]
    (assert (parser? p) (str p " is not a parser"))
    p)) 

(proxy-parser clojure.lang.Atom  (fn [a] (str (:rule-name (meta a)))) deref-parser)
(proxy-parser clojure.lang.Delay (fn [_] "~") deref-parser)

(ann-record Delta [p1 :- Parser])
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

(ann is-null-singleton? [Parser -> Boolean])
(defn is-null-singleton? [p]
  (and (is-null? p) (= 1 (count (parse-null p)))))

(ann-record Cat [p1 :- Parser, p2 :- Parser])
(defrecord Cat [p1 p2]
  Parser
  (-graph-label [_] "cat")
  (-children [_] [p1 p2])
  (-parse-null [_]
    (for> :- ParseTree
          [t1 :- ParseTree (parse-null p1),
           t2 :- ParseTree (parse-null p2)]
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
          (ann-form #(with-meta (make-parse-tree (first (parse-null p1)) %)
                       {:merge true})
                    ReducerFn))

     (is-null-singleton? p2)
     (red (compact p1)
          (ann-form #(with-meta (make-parse-tree % (first (parse-null p2)))
                       {:merge true})
                    ReducerFn))

     :default (cat' (compact p1) (compact p2))))
)

(ann-record Alt [p1 :- Parser, p2 :- Parser])
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

(ann-record Star [p1 :- Parser])
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

(ann-record Red [p1 :- Parser, f :- ReducerFn])
(defrecord Red [p1 f]
  Parser
  (-graph-label [_] [(str "→"
                          (try (graph/escape-string (pr-str (f \%)))
                               (catch Exception e "_")))])
  (-children [_] #{p1})
  (-parse-null [_] (map f (parse-null p1)))
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


(ann graph-size [Parser -> AnyInteger])
(defn graph-size [p]
  (count (graph/bfs children p)))

(ann ^:no-check view-parser-graph [Parser -> Any])
(defn view-parser-graph [p]
  (view-graph 
   (graph/bfs children p) children 
   :node->descriptor (fn [n] {:label (graph-label n)})))


(def-alias Grammar (Map Keyword (U Parser Keyword)))
(def-alias GrammarVec (Vec (U Parser Keyword)))

(ann ^:no-check grammar-vec?
  [Any -> Boolean :filters {:then (is GrammarVec 0), :else (! GrammarVec 0)}])
(defn grammar-vec? [x]
  (and (vector? x) (keyword? (first x))))

(ann label [Keyword Parser -> Red])
(defn label [tag parser]
  (red parser (fn> :- ParseTree [parse-tree :- ParseTree]
                (if (= parse-tree ::no-result)
                  ::no-result
                  (make-parse-tree tag parse-tree)))))

(ann drop-last-kw-char [Keyword -> Keyword])
(defn drop-last-kw-char [kw]
  (keyword (apply str (drop-last (name kw)))))

(ann is-hidden-rule? [Keyword -> Boolean])
(defn is-hidden-rule? [rule]
  (= \- (last (name rule))))

(ann normalize-rule [Keyword -> Keyword])
(defn normalize-rule [rule]
  (if (is-hidden-rule? rule)
    (drop-last-kw-char rule)
    rule))

(ann ^:no-check grammar->parser
     (Fn [GrammarVec -> Parser]
         [Grammar Keyword -> Parser]))
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

(ann coerce-parser [(U GrammarVec Parser) -> Parser])
(defn coerce-parser [grammar-or-parser]
  (if (grammar-vec? grammar-or-parser)
    (grammar->parser grammar-or-parser)
    grammar-or-parser))


(ann full-derivative [Parser (Seqable Token) -> Parser])
(defn full-derivative [parser input]
  ;; (view-parser-graph parser)
  (if-let [c (first input)]
    (let [d (derivative parser c)
          d' (compact d)]
      (recur d' (rest input)))
    parser))


(ann parse (Fn [(U Parser GrammarVec) (Seqable Token) -> ParseForest]
               [Grammar Keyword (Seqable Token) -> ParseForest]))
(defn parse
  ([parser input]
     (let [parser (coerce-parser parser)]
       (parse-null (full-derivative parser input))))
  ([g p input]
     (let [p (grammar->parser g p)]
       (parse p input))))
