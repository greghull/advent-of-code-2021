(ns advent-2021.day-2
  (:require [clojure.string :as str]))

;; This is the solution to part 2.  Part 1 was similar but simpler.

(defn forward [[x y aim] n]
  [(+ x n) (+ y (* aim n)) aim])

(defn up [[x y aim] n]
  [x y (- aim n)])

(defn down [[x y aim] n]
  [x y (+ aim n)])

(defn eval-file [f]
  (->> f slurp str/split-lines (str/join ")(") (#(str "(-> [0 0 0] (" % "))")) load-string))

(let [[x y aim] (eval-file "resources/day_2.txt")]
  (* x y))
