(ns advent-2021.day-1)

(defn read-seq
  "Read in a file with a list of integers into a sequence"
   [fname]
  (->> fname
       slurp
       (#(str "[" % "]"))
       read-string))

(defn increases
  "Given a sequence `v` of n numbers, returns list of n-1 booleans
  determined by v[i] < v[i+1]"
  ([v] (increases v []))
  ([v results]
   (let [[a b] v]
     (if (nil? b)
       results
       (recur (rest v) (conj results (< a b)))))))

(defn sliding-windows
  ([v] (sliding-windows v []))
  ([v wins]
   (let [[a b c] v]
     (if (nil? c)
       wins
       (recur (rest v) (conj wins (+ a b c)))))))

;; Part 1
(->> "resources/day_1.txt"
     read-seq
     increases
     (filter true?)
     count)

;; Part 2
(->> "resources/day_1.txt"
     read-seq
     sliding-windows
     increases
     (filter true?)
     count)
