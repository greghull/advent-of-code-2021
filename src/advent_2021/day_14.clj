(ns advent-2021.day-14
  (:require [clojure.string :as str]))

(defn parse-rule [rule]
  {(str (first rule) (second rule))
   [(str (first rule) (nth rule 6))
    (str (nth rule 6) (second rule))]})

(defn inc* [x]
  (if (nil? x) 1 (inc x)))

(defn get-pairs [start]
  (loop [pairs {}
         coll start]
    (if (second coll)
      (recur (update pairs (str (first coll) (second coll)) inc*) (rest coll))
      pairs)))

(defn insert [rules [key n]]
  (let [p (get rules key)]
    (if (= (first p) (second p))
      {(first p) (* 2 n)}
      {(first p) n (second p) n})))

(defn step [rules pairs]
  (reduce (partial merge-with +) (map (partial insert rules) pairs)))

(defn split-pair [[p n]]
  (if (= (first p) (second p))
    {(first p) (* 2 n)}
    {(first p) n (second p) n}))

(let [lines (str/split-lines (slurp "resources/day_14.txt"))
      start (first lines)
      pairs (get-pairs start)
      rules (reduce merge {} (map parse-rule (nthrest lines 2)))
      result (last (take 41 (iterate (partial step rules) pairs)))
      elements (reduce (partial merge-with +) (map split-pair result))
      counts (map second elements)
      big (apply max counts)
      lil (apply min counts) ]
  (/ (inc (- big lil)) 2))