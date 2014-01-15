(ns parsing-with-derivatives.protocols)

(defprotocol Parser
  (-graph-label [this]   "The parser's label in the debug graph. ")
  (-children    [this]   "A sequence of the parsers referred to directly by this one")
  (-parse-null  [this]   "Run the null parse; i.e. get the results out of this parser.")
  (-derivative  [this c] "Parser derivative.")
  (-is-null?    [this]   "Does this parser accept the empty string?")
  (-is-empty?   [this]   "Is this the empty parser, or equivalent?")
  (-compact     [this]   "Parser compaction"))

