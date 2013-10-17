(ns parsing-with-derivatives.typecheck
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [clojure.test :refer :all]
            [parsing-with-derivatives.core]
            [clojure.core.typed :refer [check-ns]]))

(deftest test-types
  (is (check-ns 'parsing-with-derivatives.core)))
