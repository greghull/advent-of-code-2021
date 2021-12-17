(ns advent-2021.day-17)

;target area: x=185..221, y=-122..-74
(def target {:x1 185 :x2 221 :y1 -122 :y2 -74})

(defn drag-x [vx]
  (cond
    (> vx 0) (dec vx)
    (< vx 0) (inc vx)
    (= vx 0) 0))

(defn fire [target [init_vx init_vy]]
  (loop [x 0
         y 0
         max_y 0
         vx init_vx
         vy init_vy]
    (cond (< y (:y1 target)) nil
          (and (>= x (:x1 target)) (<= x (:x2 target))
            (>= y (:y1 target)) (<= y (:y2 target)))
          {:x x :y y :vx vx :vy vy :max_y max_y :init_vx init_vx :init_vy init_vy}
          :else (recur (+ x vx) (+ y vy) (max max_y y) (drag-x vx) (dec vy)))))

; Part 1
(->> (for [x (range 0 300)
           y (range 0 300)]
       [x y])                     ; generate sequence of initial values
  (pmap #(fire target %))         ; map them to firing outcome
  (filter identity)               ; filter out the misses
  (sort-by :max_y >)              ; sort by max height
  first)                          ; first one is the answer

; Part 2
(->> (for [x (range 0 500) y (range -300 300)]
       [x y])                     ; generate sequence of initial values
  (pmap #(fire target %))         ; map them to firing outcome
  (filter identity)               ; filter out the misses
  count)                          ; count the results