(ns parsing-with-derivatives.fixpoint
  (:refer-clojure :exclude [empty] :rename {empty? empty-coll?})
  (:require [clojure.set :as s]))

(def ^:dynamic *running* #{})
(def ^:dynamic *cache*)
(def ^:dynamic *changed?*)
(def ^:dynamic *visited*)

(defn fix-memo [bottom f]
  (letfn [(inner [x] 
            (let [cache-key [f x]
                  cached? (contains? @*cache* cache-key)
                  cached (get @*cache* cache-key bottom)]
              (if (contains? @*visited* cache-key)
                (if cached? cached bottom)
                (do
                  (swap! *visited* conj cache-key)
                  (let [new-val (f x)]
                    (when (not= new-val cached)
                      (reset! *changed?* true)
                      (swap! *cache* assoc cache-key new-val))
                    new-val)))))]
    (fn [x]
      (if (*running* f)
        (inner x)
        (binding [*cache* (atom {})
                  *changed?* (atom true)
                  *running* (conj *running* f)
                  *visited* (atom #{})]
          (let [v (atom bottom)]
            (while @*changed?*
              (reset! *changed?* false)
              (reset! *visited* #{})
              (reset! v (inner x)))
            @v))))))

