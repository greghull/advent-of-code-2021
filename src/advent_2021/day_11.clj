```clojure
(ns advent-2021.day-11
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]))

(def n-flashes (atom 0))

(defn input []
  (->> "resources/day_11.txt"
    slurp
    str/split-lines
    (map #(str/split % #""))
    (m/matrix)
    (m/emap read-string)))

(defn hood [[row col]]
  (filter #(not= %1 [row col])
    (for [x (range -1 2) y (range -1 2)] [(+ row y) (+ col x)])))

(defn update-status [energy pos s]
  (case s
    nil (when (> (get-in energy pos) 9) :flashing)
    :flashing :flashed
    :flashed :flashed))

(defn update-energy [status pos e]
  (let [n (count (filter #(= % :flashing) (map #(get-in status %1 nil) (hood pos))))]
    (+ e n)))

(defn reset-energy [e]
  (if (> e 9) 0 e))

(defn step [energy]
  (loop [e (m/emap inc energy)
         s (m/emap-indexed (partial update-status e) (m/emap (fn [_] nil) e))]
    (let [n (count (filter #(= :flashing %) (flatten s)))]
      (swap! n-flashes + n)
      (if (> n 0)
        (let [e2 (m/emap-indexed (partial update-energy s) e)]
          (recur e2 (m/emap-indexed (partial update-status e2) s)))
        (m/emap reset-energy e)))))

; Part 1
(let [energy (input)]
  (reset! n-flashes 0)
  (last (take 101 (iterate step energy)))
  @n-flashes)

; Part 2
(loop [energy (input)
       n 0]
  (if (= 0 (reduce + (flatten energy)))
    n
    (recur (step energy) (inc n))))
```

