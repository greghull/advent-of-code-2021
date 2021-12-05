(ns advent-2021.day-1)

(defn read-seq [f]                                          ; read file and parse it
  (->> f slurp (#(str "[" % "]")) read-string))

;; Part 1
(->> "resources/day_1.txt"
     read-seq                                               ;convert text to seq
     (#(map < % (rest %)))                                  ;map to s_i < s_i+1
     (filter true?)                                         ;filter out false
     count)                                                 ;count results

;; Part 2
(->> "resources/day_1.txt"
     read-seq                                               ;convert text to seq
     (#(map + % (rest %) (nthrest % 2)))                    ;map to s_i + s_i+1 + s+i+2
     (#(map < % (rest %)))                                  ;map to s_i < s_i+1
     (filter true?)                                         ;filter out false
     count)                                             ;count the results
