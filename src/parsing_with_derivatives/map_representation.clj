(ns parsing-with-derivatives.map-representation)

(defmulti derivative (fn [grammar c] (first (:S grammar))))

(defmethod derivative :char [grammar c]
  (let [[_ expected-char] (:S grammar)]
    (if (= expected-char c)
      {:S [:eps]}
      {:S [:empty]})))

(defmethod derivative :eps [grammar c]
  {:S [:empty]})

(defmethod derivative :empty [grammar c]
  {:S [:empty]})

(defmethod derivative :alt [grammar c]
  (let [[_ p1 p2] (:S grammar)]
    {:S [:alt p1 p2]
     p1 (derivative {:S (get grammar p1)} c)
     p2 (derivative {:S (get grammar p2)} c)}))

(def re
  {:S [:alt :a :b]
   :a [:char \a]
   :b [:char \b]})

(derivative re \a)
