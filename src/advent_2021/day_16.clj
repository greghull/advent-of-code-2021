(ns advent-2021.day-16
  (:require clojure.string :as str))

(def *code* {\0 "0000" \1 "0001" \2 "0010" \3 "0011"
             \4 "0100" \5 "0101" \6 "0110" \7 "0111"
             \8 "1000" \9 "1001" \A "1010" \B "1011"
             \C "1100" \D "1101" \E "1110" \F "1111"})

(defn decode [s]
  (apply str (map #(get *code* %1) s)))

(defn read [src n]
  (let [ret (Integer/parseInt (apply str (take n @src)) 2)]
    (swap! src nthrest n)
    ret))

(defn packet
  "Returns a packet of the form {:version v :type t :packets p}"
  [src]
  (let [v (read src 3)
        t (read src 3)]
    {:version v :type t}))

(let [src (atom (decode "D2FE28"))]
  (packet src))