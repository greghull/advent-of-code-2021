(ns advent-2021.day-5
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]
            [clojure.math.numeric-tower :as math]))

(defn slope
  "Given a `line` returns a map with {:rise :run}"
  [line]
  (let [rise (- (:y2 line) (:y1 line))
        run (- (:x2 line) (:x1 line))
        d (math/gcd (math/abs rise) (math/abs run))]
    {:run (/ run d) :rise (/ rise d)}))

(defn read-lines
  "Read input file and convert to an array of lines."
  [f]
  (->> f
    slurp
    (#(str/replace % #"[->,]" " ")) ; convert punctuation to whitespace
    (#(str/split % #"\s+"))         ; split at whitespace
    (map read-string)               ; convert to an array of ints
    (partition 4)                   ; partition into 4 points each
    (map #(zipmap [:x1 :y1 :x2 :y2] %)))) ; name the points x1, y1, x2, y2

(defn draw-line [line c]
  (let [{run :run
         rise :rise} (slope line)]
    (loop [x (:x1 line)
           y (:y1 line)
           c c]
      (if (and (== x (:x2 line)) (== y (:y2 line)))
        (update-in c [y x] inc)
        (recur (+ x run) (+ y rise) (update-in c [y x] inc))))))

(defn draw [lines c]
  (loop [l lines
         c c]
    (if (nil? l)
      c
      (recur (next l) (draw-line (first l) c)))))

; Filters for part1 and part 2
(defn part1-filter
  "Returns true for a horizontal or vertical line, false for all others"
  [line]
  (or (== (:x1 line) (:x2 line)) (== (:y1 line) (:y2 line))))

(defn solve
  "Counts the # of points where lines intersect"
  [lines]
  (->> (matrix/new-matrix 1000 1000)
    (draw lines)
    flatten
    (filter #(> % 1))
    count))

;; Solve Part 1
(time (solve (->> "resources/day_5.txt" read-lines (filter part1-filter))))
;; Solve Part 2
(time (solve (->> "resources/day_5.txt" read-lines)))