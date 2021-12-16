(ns advent-2021.day-15
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.matrix :as m]))

(defn hood [unvisited node]
  (let [y (first node)
        x (second node)
        h #{ [(dec y) x]
            [y (dec x)] [y (inc x)]
             [(inc y) x]}]
    (set/intersection unvisited h)))

(defn update-distances [cave distance node h]
  (if-let [x (first h)]
    (recur cave
      (assoc-in distance x (min (get-in distance x)
                             (+ (get-in distance node) (get-in cave x))))
      node
      (rest h))
    distance))

(defn next-node [unvisited distance]
  (when (not-empty unvisited)
    (apply min-key #(get-in distance %1) unvisited)))

(defn djk [cave start end]
  (loop [distance (assoc-in (m/emap (fn [_] 99999) cave) [0 0] 0)
         unvisited (into #{} (remove #(= % start)
                     (for [y (range (inc (first end)))
                           x (range (inc (second end)))]
                       [y x])))
         node start]
    (if (some #{end} unvisited)
      (let [h (hood unvisited node)
            d (update-distances cave distance node h)
            u (set/difference unvisited #{node})
            n (next-node u d)]
        (recur d u n))
      distance)))

(defn inc-risk [i n]
  (let [r (+ i n)]
    (if (> r 9) (- r 9) r)))

(defn make-taller [m]
  (apply concat
    (for [i (range 5)]
      (m/emap #(inc-risk i %) m))))

; Part 2
(let [lines (str/split-lines (slurp "resources/day_15.txt"))
      seqs (map #(apply str (interpose " " %)) lines)
      cave (m/matrix (map #(read-string (str "[" % "]")) seqs))
      biggie (m/transpose (make-taller (m/transpose (make-taller cave))))]
  ; Part 1
  ; (djk cave [0 0] [(dec (m/row-count cave)) (dec (m/column-count cave))])
  ; Part 2
  (get-in (djk biggie [0 0] [(dec (m/row-count biggie)) (dec (m/column-count biggie))])
    [499 499]))
