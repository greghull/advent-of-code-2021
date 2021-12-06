(ns advent-2021.day-6)

(def school
  ; The initial school of fish given from the input
  ; Modeled as a hashmap of age -> # of fish of that age.
  (let [fish (zipmap (range 9) (repeat 9 0))   ; initially 0 fish at each day of cycle
        input (->> "resources/day_6.txt" slurp (#(str "[" % "]")) read-string)]
    (reduce #(update %1 %2 inc) fish input)))

(defn reproductive-cycle
  "Given a school of fish, looks only at fish on day `x` of their cycle and returns
  a map of fish on the next stage of the cycle.  On day 0 new fish are added to the
  school"
  [fish x]
  (let [n (get fish x)]     ; # of fish on day x of their cycle
    (if (zero? x) {6 n 8 n} {(dec x) n})))

(defn day
  "Given a school of fish, returns the larger school after a single day has passed"
  [fish]
  (reduce (partial merge-with +) {} (map #(reproductive-cycle fish %) (keys fish))))

(defn simulate
  "Given a school of fish, returns the larger school after n days have passed"
  [n fish]
  (reduce (fn [f _] (day f)) fish (range n)))

(->> school (simulate 80) vals (reduce +))                  ; Part 1
(->> school (simulate 256) vals (reduce +))                 ; Part 2