(ns advent-2021.day-7)

(defn consumed [start end]                         ; part 1 fuel consumption
  (if (> start end) (- start end) (- end start)))  ; clojure has no built-in abs

(defn consumed2 [start end]                        ; part 2 fuel consumption
  (reduce + (range 0 (inc (consumed start end)))))

(defn get-fuel [f coll p]         ; get the amount of fuel used to move all crabs
  (reduce + (map #(f p %) coll))) ; to position `p using consumption function `f`

(def initial (->> "resources/day_7.txt" slurp (#(str "[" % "]")) read-string))
(def possible (range (apply min initial) (apply max initial)))

(apply min (pmap (partial get-fuel consumed initial) possible))  ; part 1 answer
(apply min (pmap (partial get-fuel consumed2 initial) possible)) ; part 2 answer