(ns parsing-with-derivatives.protocols
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [clojure.core.typed :refer :all])
  (:import [clojure.lang Keyword]))

;; General parser types
;; In principle this whole library can be generic over the token type
(def-alias Token Character)
(def-alias ParseTree (Option (U Token (Seqable ParseTree) (Value :parsing-with-derivatives.core/no-result))))
(def-alias ParseForest (Seqable ParseTree))
(def-alias ReducerFn [ParseTree -> ParseTree])

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

