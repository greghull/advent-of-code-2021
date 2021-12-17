(ns advent-2021.day-17)

(def target {:x1 185 :x2 221 :y1 -122 :y2 -74})

(defn drag-x [vx]
  (cond
    (> vx 0) (dec vx)
    (< vx 0) (inc vx)
    (= vx 0) 0))

(defn hit? [target x y]
  (and (>= (:x2 target ) x (:x1 target)) (>= (:y2 target) y (:y1 target))))

(defn fire [target [init_vx init_vy]]
  (loop [x 0 y 0 max_y 0 vx init_vx vy init_vy]
    (cond (< y (:y1 target)) nil  ; we missed the target range
          (hit? target x y) max_y ; hit, return the max height
          :else (recur (+ x vx) (+ y vy) (max max_y y) (drag-x vx) (dec vy)))))

; Part 1
(->> ; generate sequence of initial velocities
  (for [x (range 0 300) y (range 0 300)] [x y])
  (pmap #(fire target %))         ; map them to firing outcome
  (filter identity)               ; filter out the misses
  (sort >)                        ; sort by max height
  first)                          ; first one is the answer

; Part 2
(->> ; generate sequence of initial velocities
  (for [x (range 0 300) y (range -300 300)] [x y])
  (pmap #(fire target %))         ; map them to firing outcome
  (filter identity)               ; filter out the misses
  count)                          ; count the results