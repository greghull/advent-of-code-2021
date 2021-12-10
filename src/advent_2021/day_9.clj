(ns advent-2021.day-9
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.matrix :as matrix]))

(defn input
  "Read the file `f` into a matrix of ints"
  [f]
  (->> f slurp str/split-lines
    (map #(str/split % #""))
    (map #(map read-string %))
    (matrix/matrix)))

(defn low-point?
  "Return the point [row point] when [row col] is a low point, otherwise return nil"
  [m row col]
  (let [left (get-in m [row (dec col) ] 9)
        right (get-in m [row (inc col)] 9)
        up (get-in m [(dec row) col] 9)
        down (get-in m [(inc row) col] 9)
        here (get-in m [row col])]
    (when (and (< here left) (< here right) (< here up) (< here down))
      [row col])))

(defn hood
  "return the immediate neighborhood of points around p that are in the same basin
  as the point [row col]"
  [m [row col]]
  (let [pts [[(dec row) col] [(inc row) col]
             [row (dec col)] [row (inc col)]]]
    (into #{} (filter #(< (get-in m % 9) 9) pts))))

(defn expand
  "Expand a basin by 1 step in all directions"
  [m basin]
  (reduce set/union basin (map (partial hood m) basin)))

(defn basin-size
  "Keep expanding the basin centered on the point `p`, then return it's size"
  [m p]
  (let [basin (iterate (partial expand m) #{p})]
    (count (nth basin 10))))

; part 2
(let [m (input "resources/day_9.txt")                 ; get the matrix from input file
      lows (for [row (range (matrix/row-count m))     ; find the low points
                 col (range (matrix/column-count m))]
             (low-point? m row col))
      lows (filter some? lows)]                       ; remove the nils
  (->> lows
    (map (partial basin-size m))                      ; map each low to its basin size
    (sort #(compare %2 %1))                           ; sort, largest first
    (take 3)                                          ; take the first 3
    (reduce *)))                                      ; return the product of those 3