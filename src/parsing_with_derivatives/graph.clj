(ns parsing-with-derivatives.graph
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [clojure.set :as s]
            [clojure.string :as string]
            [clojure.core.typed :refer :all]))

;; from http://hueypetersen.com/posts/2013/06/25/graph-traversal-with-clojure/
(ann ^:no-check bfs (All [x] [(Fn [x -> (Seqable x)]) x -> (ISeq x)]))
(defn bfs [neighbors-fn start]
  ((fn rec-bfs [explored frontier]
     (lazy-seq
      (when (seq frontier)
        (let [_ (print-env "bfs")
              v (peek frontier)
              neighbors (neighbors-fn v)]
          (cons v (rec-bfs
                   (into explored neighbors)
                   (into (pop frontier) (remove explored neighbors))))))))
   #{start}
   (conj (clojure.lang.PersistentQueue/EMPTY) start)))


(ann escapable-characters String)
(def ^:private escapable-characters "|{}\"")

(ann ^:no-check escape-string [String -> String])
(defn escape-string
  "Escape characters that are significant for the dot format."
  [s]
  (reduce
    #(string/replace %1 (str %2) (str "\\" %2))
    s
    escapable-characters))
