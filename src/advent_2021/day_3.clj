(ns advent-2021.day-3
  (:require [clojure.string :as str]))

(defn read-seq [f]                                          ; read file and parse it
  (->> f slurp str/split-lines))

;; Part 1

(defn digits [v]
  (mapv #(Character/digit % 2) v))

(defn greeks [v]
  (let [t (reduce + v)]
    (if (> t (/ (count v) 2))
      [1 0]
      [0 1])))

(->> "resources/day_3.txt"
  read-seq
  (apply mapv vector)                                       ;transpose the vectors
  (mapv digits)                                             ;convert to digits
  (mapv greeks)                                             ;map to gamma/epsilon
  (apply mapv vector)                                       ;transpose back
  (mapv #(reduce str %))                                    ;convert to binary string
  (mapv #(Integer/parseInt % 2))                            ;parse the strings
  (reduce *))                                               ;Multiply for answer

;; Part 2

(defn group-values
  "Split a vector of bit-strings into 2 groups based on the requirements of `op` at
  position `n`"
  [n op v]
  (let [m (group-by #(nth % n) v)
        ones (get m \1)
        zeros (get m \0)]
    (if (op (count ones) (count zeros))
      ones
      zeros)))

(defn filter-values
  "recursively process a vector of bit strings on all bit positions"
  [op v]
  (loop [v v
         n 0]
    (if (= 1 (count v))
      v
      (recur (group-values n op v) (inc n)))))


(apply *
  (for [op [>= <]]                                          ; o2 is >=, co2 is <
    (->> "resources/day_3.txt"
        read-seq
        (filter-values op)                                  ; filter based on element
        first                                               ; take last string standing
        (#(Integer/parseInt % 2))))) ; parse it


(->> 100 range (filter even?) (reduce +))