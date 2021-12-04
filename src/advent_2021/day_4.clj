(ns advent-2021.day-4
  (:require [clojure.string :as str]))

(def input (->> "resources/day_4.txt" slurp str/split-lines))
(def numbers (-> input first (#(str "[" % "]")) read-string))

;; Given 5 strings of 5 numbers, return a 5x5 matrix
(defn make-board [coll]
  (mapv #(read-string (str "[" % "]")) coll))

(def boards
  (->> input rest        ; ignore first line
    (filter not-empty)   ; remove blank lines
    (partition 5)        ; break into vectors of 5 strings
    (mapv make-board)))  ; create the boards

(defn call
  "Call the number n. If n is contained on the board, replace it with a 0."
  [n board]
  (for [row board]
    (mapv #(if (= n %) 0 %) row)))

(defn winner? [board]
  (or (.contains board [0 0 0 0 0])                       ; winner if there is a row of 0's
    (.contains (apply mapv vector board) [0 0 0 0 0])))   ; or it's transpose has a row of 0's

(defn first-winner [boards numbers]
  (let [n (first numbers)                        ; get the current number
        b (map #(call n %) boards)               ; call that number for each board
        winner (.indexOf (map winner? b) true)]  ; look for a winning board
    (if (>= winner 0)
      [(nth b winner) n]                         ; return the winning board and n
      (recur b (rest numbers)))))                ; keep playing!

(defn last-winner [boards numbers last-winner]
  (let [n (first numbers)                        ; get the current number
        b (map #(call n %) boards)               ; call the number for each board
        loser (.indexOf (map winner? b) false)]  ; find a losing board
    (if (>= loser 0 )
      (recur b (rest numbers) loser)             ; keep playing if we have losers
      [(nth b last-winner) n])))                 ; no losers, return the last winner!

(defn score [b n]
  (->> b flatten (reduce +) (* n)))

;; Part 1 Answer
(apply score (first-winner boards numbers))
;; Part 2 Answer
(apply score (last-winner boards numbers -1))