ns advent-2021.day-13
  (:require [clojure.string :as str]))

(defn reflect-point [[axis val] [x y]]
  (let [n (read-string val)]
    (case axis
      "x" [(if (> x n) (- n (- x n)) x) y]
      "y" [x (if (> y n) (- n (- y n)) y)])))

(defn fold [rule dots]
  (distinct (map #(reflect-point rule %) dots)))

(defn fold-all [folds dots]
  (if (nil? folds)
    dots
    (recur (next folds) (fold (first folds) dots))))

(defn draw [dots]
  (for [y (range 15)]
    (apply str (for [x (range 50)]
      (if (some #{[x y]} dots) \# \space)))))

;; Part 2
(let [lines (str/split-lines (slurp "resources/day_13.txt"))
      dots (map #(read-string (str "[" % "]")) (filter #(str/index-of % ",") lines))
      folds (map rest (filter some? (map #(re-matches #".+([xy]+)=(\d+)" %) lines)))
      final (fold-all folds dots)]
  (draw final))