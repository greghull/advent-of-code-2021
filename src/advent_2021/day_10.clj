(ns advent-2021.day-10
  (:require [clojure.string :as str]))

(def openers "[({<")
(def closers {\( \) \[ \] \< \> \{ \}})
(def score {\) 3 \] 57 \} 1197 \> 25137
            \( 1 \[ 2  \{ 3    \< 4})

(defn parse1
  "Parse a line.  If the wrong closing character is encountered return it,
  otherwise return nil"
  [line]
  (loop [ch (first line)
         l (rest line)
         opens '()]
    (when (some? ch)
      (cond
        (str/index-of openers ch) (recur (first l) (rest l) (conj opens ch))
        (= (closers (first opens)) ch) (recur (first l) (rest l) (rest opens))
        :else ch))))

;; Part 1
(->> "resources/day_10.txt"
  slurp             ; read file
  str/split-lines   ; split lines
  (map parse1)      ; map line to error ch or nil
  (filter some?)    ; filter out the nils
  (map score)       ; map bad chars to score
  (reduce +))       ; sum the scores

(defn parse2
  "Parse a line.  Return nil on invalid lines, on incomplete lines return a
  sequence of the dangling opening brackets."
  [line]
  (loop [ch (first line)       ; the char we are looking at
         l (rest line)         ; the rest of the line
         opens '()]            ; a list of opening characters so far
    (if (some? ch)             ; do we have another character?
      (cond                    ; If statement is true, let's process it
        (str/index-of openers ch) (recur (first l) (rest l) (conj opens ch))
        (= (closers (first opens)) ch) (recur (first l) (rest l) (rest opens))
        :else nil)             ; invalid closer found, return nil
      opens)))                 ; If statement is false, return the unmatched openers

(defn score-line
  "Implements the scoring rule for Part 2"
  [c]
  ;; maps each closing character to it's score, then multiplies/adds as per the rules
  (reduce #(+ %2 (* 5 %1)) 0 (map score c)))

;; Part 2
(let [scores (->> "resources/day_10.txt"
               slurp                        ; read file
               str/split-lines              ; split into lines
               (map parse2)                 ; map line to unmatched openers
               (filter some?)               ; filter out the nils
               (map score-line)            ; map each line to it's score
               (#(into [] (sort %1))))      ; sort the scores
      n (Math/floor (/ (count scores) 2))]  ; find the mid-point
  (nth scores n))                           ; return mid score