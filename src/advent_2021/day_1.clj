(ns advent-2021.day-1)

(defn read-seq
  "Read in a file with a list of integers into a sequence"
   [fname]
  (->> fname
       slurp
       (#(str "[" % "]"))
       read-string))

(defn increases
  "Given a collection of n numbers, returns list of n-1 booleans
  determined by v[i] < v[i+1]"
  ([coll] (increases coll []))
  ([coll results]
   (let [[a b] coll]
     (if (nil? b)
       results
       (recur (rest coll) (conj results (< a b)))))))


(defn sliding-windows
  "Returns a sequence of the sums of windows `n` elements from the
  collection `coll`"
  ([n coll] (sliding-windows n coll []))
  ([n coll wins]
   (let [window (take n coll)]
     (if (> n (count window))
       wins
       (recur n (rest coll) (conj wins (reduce + window)))))))

;; Part 1
(->> "resources/day_1.txt"
     read-seq
     increases
     (filter true?)
     count)

;; Part 2
(->> "resources/day_1.txt"
     read-seq
     (sliding-windows 3)
     increases
     (filter true?)
     count)
