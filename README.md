# parsing-with-derivatives

A Clojure implementation of the parsing with derivatives algorithm.

Parsing with derivatives is interesting because it can handle
arbitrary context-free grammars, without regard to left- or right-
recursion.

If you just want to parse something, you shouldn't be here yet! Take a look
at https://github.com/Engelberg/instaparse.

## Usage

### Grammars

A grammar is defined as a map of alternating rule names and
definitions. A rule name is a keyword, a definitions are formed of terminals
and combinators. The first rule in the vector is the start rule.

| Terminal        | Meaning             |
|-----------------|---------------------|
| eps             | Epsilon, the parser which acceps the empty string. |
| <character>     | A character literal is a parser which accepts itself. |
| <keyword>       | A reference to the named rule |

| Combinator      | Meaning              |
|-----------------|---------------------|
| (cat p1 p2...)  | Concatentation, i.e. sequence. Variadic. |
| (alt p1 p2)     | Alternation, a choice between two parsers. |
| (star p1)       | Kleene star, zero or more of p1. |
| (plus p1)       | Kleene plus, one oor more of p1. |
| (red p1 f)      | Reduction, run the parse tree of p1 through f. |
| (hide p1)       | Don't include the parse tree of p1. |

*TODO*: alt should be variadic as well

### Parsing

Use the 'parse' fuction as ```(parse grammar input)```. It returns a sequence
of parse trees; if your grammar is ambiguous you will get more than one.

*TODO*: The above is true in principle, but has not been verified.
*TODO*: Change this to a parse/parses pair like instaparse.

### Controlling the parse tree

One parse tree node is generated for every rule in the grammar, with a
keyword label as the first element (enlive-style). For example, using
the grammar ```[:S (cat \a :T), :T (cat \b \c)]``` to parse the string "abc"
gives the parse tree ```[:S \a [:T \b \c]]```.

If you name a rule using a '-' as the last character, its parse tree
will be merged into the rule which invoked it. Changing the previous example,
```[:S (cat \a :T), :T- (cat \b \c)]``` will parse the string "abc" to the
parse tree ```[:S \a \b \c]```.

## Examples

Arithmetic expression parser:

```clojure
(def expression
  [:expr :add-expr
   :add-expr- (alt :mult-expr
                   (label :add (cat :mult-expr (hide :add-op) :mult-expr)))
   :add-op (alt \+ \-)
   :mult-expr- (alt :value
                    (label :mult (cat :value (hide :mult-op) :value)))
   :mult-op (alt \* \/)
   :value- (alt :number
                (cat (hide \() :add-expr (hide \))))
   :number- (red (plus :digit) #(Integer/parseInt (apply str %)))

   :digit- (reduce alt [\1 \2 \3 \4 \5 \6 \7 \8 \9 \0])])
```

Usage:

```clojure
(parse expression "1*(2+3)") ; => [:expr [:mult 1 [:add 2 3]]]
```

## Deficiencies

- Performance has not been tested and is probably bad.
- Only literal terminals are supported; no ranges, no regular
  expressions.

## References

* A lot of inspiration is taken from instaparse, which is the parser
  you should probably be using:

  https://github.com/Engelberg/instaparse

* Some other ideas have been appropriated from parsely, the other
  parser you should probably be using:

  https://github.com/cgrand/parsley

* The parsing with derivatives paper and associated web site:

  http://matt.might.net/papers/might2011derivatives.pdf

  http://matt.might.net/articles/parsing-with-derivatives/

* A lecture about parsing with derivatives:

  http://www.youtube.com/watch?v=ZzsK8Am6dKU

* A well-documented java implementation:

  http://maniagnosis.crsr.net/p/parsing-with-derivatives.html

* Another clojure implementation:

  https://github.com/cstorey/parsing-derivatives-experiment

## License

Copyright © 2013 Russell Mull

Except walk2.clj, which was written by Stuart Sierra for hopeful inclusion
into a future version of Clojure. https://github.com/stuartsierra/clojure.walk2

Distributed under the Eclipse Public License, the same as Clojure.
