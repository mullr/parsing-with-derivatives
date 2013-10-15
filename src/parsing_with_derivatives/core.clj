(ns parsing-with-derivatives.core
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [clojure.core.typed :refer :all]
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
(ann ^:no-check clojure.core/drop
     (All [x] [AnyInteger (Seqable x) -> (Seq x)]))

;; Annotate internal libraries
(ann ^:no-check parsing-with-derivatives.fixpoint/fix-memo
     (All [x y] [y [x -> y] -> [x -> y]]))

;; General parser types
;; In principle this whole library can be generic over the token type
(def-alias Token Character)
(def-alias ParseTree (Option (U Token (Seqable ParseTree))))
(def-alias ParseForest (Seqable ParseTree))
(def-alias ReducerFn [ParseTree -> ParseTree])

(ann parse-tree [ParseTree ParseTree -> ParseTree])
(defn parse-tree "Parse tree constructor" [x y]
  [x y])

(ann-protocol Parser
  -graph-label [Parser -> (U String (Vec String))]
  -children [Parser -> (Seqable Parser)]
  -parse-null [Parser -> ParseForest]
  -derivative [Parser Token -> Parser]
  -is-null? [Parser -> Boolean]
  -is-empty? [Parser -> Boolean]
  -compact [Parser -> Parser])

(defprotocol> Parser
  (-graph-label [this]   "The parser's label in the debug graph. ")
  (-children    [this]   "A sequence of the parsers referred to directly by this one")
  (-parse-null  [this]   "Run the null parse; i.e. get the results out of this parser.")
  (-derivative  [this c] "Parser derivative.")
  (-is-null?    [this]   "Does this parser accept the empty string?")
  (-is-empty?   [this]   "Is this the empty parser, or equivalent?")
  (-compact     [this]   "Parser compaction"))

(ann ^:no-check parser?
     [Any -> Boolean :filters {:then (is Parser 0), :else (! Parser 0)}])
(defn parser?
  "Parser type check; useful in assertions, for helping core.typed."
  [x]
  (extends? Parser (type x)))

;; parser constructors
(declare empty eps eps* red)
(declare ->Cat ->Alt ->Delta ->Star ->Red)

(ann cat [Parser Parser -> Cat])
(defn cat [p1 p2] (->Cat p1 p2))

(ann alt [Parser Parser -> Alt])
(defn alt [p1 p2] (->Alt p1 p2))

(ann delta [Parser -> Delta])
(defn delta [p1] (->Delta p1))

(ann star [Parser -> Star])
(defn star [p1] (->Star p1))

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
  (-graph-label [_] ["ε" (graph/escape-string (str val))])
  (-children [_] #{})
  (-parse-null [_] val)
  (-derivative [_ c] empty)
  (-is-empty? [_] false)
  (-is-null? [_] true)
  (-compact [this] this))

(ann eps Epsilon)
(def eps (->Epsilon '(nil)))

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

(defmacro proxy-parser [type graph-label proxy-fn]
  `(extend-protocol Parser 
     ~type
     (-graph-label [this#] ~graph-label)
     (-children [this#] [(~proxy-fn this#)])
     (-parse-null [this#] (parse-null (~proxy-fn this#)))
     (-derivative [this# c#] (derivative (~proxy-fn this#) c#))
     (-is-empty? [this#] (is-empty? (~proxy-fn this#)))
     (-is-null? [this#] (is-null? (~proxy-fn this#)))
     (-compact [this#] (compact (~proxy-fn this#)))))

(ann deref-parser [(clojure.lang.IDeref Any) -> Parser])
(defn deref-parser [r]
  (let [p (deref r)]
    (assert (parser? p) (str p " is not a parser"))
    p)) 

(proxy-parser clojure.lang.Atom  "@" deref-parser)
(proxy-parser clojure.lang.Delay "~" deref-parser)

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
          (parse-tree t1 t2)))

  (-derivative [_ c] (alt (cat' (delta p1) (derivative p2 c))
                          (cat' (derivative p1 c) p2)))

  (-is-empty? [_] (or (is-empty? p1) (is-empty? p2)))
  (-is-null? [_] (and (is-null? p1) (is-null? p2)))
  (-compact [this] 
    (cond
     ;; the below rule is bogus!
     ;; (and (is-null? p1) (is-null? p2)) (parse-null this)

     (is-null-singleton? p1)
     (red (compact p2) (ann-form #(parse-tree (first (parse-null p1)) %)
                                  ReducerFn))

     (is-null-singleton? p2)
     (red (compact p1) (ann-form #(parse-tree % (first (parse-null p2)))
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
  (-parse-null [_] [nil])
  (-derivative [this c] (cat' (derivative p1 c) this))
  (-is-empty? [_] false)
  (-is-null? [_] (or (is-null? p1) (is-empty? p1)))
  (-compact [this] this)
)

(ann ^:no-check fn-name [Any -> String])
(defn fn-name [fn]
  (->> (str fn)
       (partition-by #{\$ \@})
       (map (partial apply str))
       (drop 2)
       (first)))

(ann-record Red [p1 :- Parser, f :- ReducerFn])
(defrecord Red [p1 f]
  Parser
  (-graph-label [_] [(str "→" (graph/escape-string (str (f \%))))])
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
     (red (compact (:p1 p1)) (comp (:f p1) f))
     
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

(ann ^:no-check grammar->parser [Grammar Keyword -> Parser])
(defn grammar->parser
  "Turn a grammar using keywords for rules into a parser. Each rule
   gets an atom, allowing for recursion. " 
  [grammar start-rule]
  (let [atomized-grammar (into {} (for [[rule production] grammar]
                                    [rule (atom production)]))]
    (doseq [[rule production-atom] atomized-grammar]
      (swap! production-atom #(walk/postwalk-replace atomized-grammar %)))
    (atomized-grammar start-rule)))


(ann full-derivative [Parser (Seqable Token) -> Parser])
(defn full-derivative [parser input]
  (if-let [c (first input)]
    (let [d (derivative parser c)
          d' (compact d)]
      (recur d' (rest input)))
    parser))


(ann parse (Fn [Parser (Seqable Token) -> ParseForest]
               [Grammar Keyword (Seqable Token) -> ParseForest]))
(defn parse
  ([parser input]
     (parse-null (full-derivative parser input)))
  ([g p input]
     (let [p (grammar->parser g p)]
       (parse p input))))
