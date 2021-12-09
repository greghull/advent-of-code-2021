(ns advent-2021.day-8
  (:require [clojure.string :as str]))

(defn parse []
  (->> "resources/day_8.txt"
    slurp
    str/split-lines
    (map #(str/split % #" \| "))
    (map #(zipmap [:signal :display] %))
    (map #(assoc % :signal (str/split (:signal %) #" ")))
    (map #(assoc % :display (str/split (:display %) #" ")))))

(defn sort-key [k x]
  (assoc x k (into []  (map #(apply str (sort %)) (k x)))))

(defn easy-digits [_ s]
  (case (count s)
    2 1
    4 4
    3 7
    7 8
    nil))

(defn in? [a b]
  (cond
    (nil? a) true
    (str/index-of b (first a)) (recur (next a) b)))

(defn six-segment-digits [x s]
  (if (= 6 (count s))
    (cond
      (in? (get-in x [:plain 4]) s) 9
      (in? (get-in x [:plain 7]) s) 0
      :else 6)
    nil))

(defn five-segment-digits [x s]
  (if (= 5 (count s))
    (cond
      (in? (get-in x [:plain 7]) s) 3
      (in? s (get-in x [:plain 9])) 5
      :else 2)
    nil))

(defn rules [f x]
  (assoc x :plain
           (merge
             (get x :plain)
             (dissoc (zipmap (map (partial f x) (:signal x)) (:signal x)) nil))))

(defn display [x]
  (map #(get-in x [:lookup %]) (:display x)))

(->>  ; part 2
  (parse)
  (map (partial sort-key :signal))
  (map (partial sort-key :display))
  (map (partial rules easy-digits))
  (map (partial rules six-segment-digits))
  (map (partial rules five-segment-digits))
  (map #(assoc % :lookup (clojure.set/map-invert (:plain %))))
  (map display)
  (map #(apply str %))
  (map #(Integer/parseInt %))
  (reduce +))