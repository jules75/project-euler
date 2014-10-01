(ns euler.p58
	(:require [euler.prime :as p])
    )

(defn- spiral-diagonals
    "Starting with 1 in the middle, build an anti-clockwise
    square spiral. This function returns an infinite series of
    the values that form the diagonals."
    []
    (->> (range)
         rest
         (filter even?)
         (mapcat (partial repeat 4))
         (reductions + 1)
         ))

(defn- solve
    [prime-count total-count coll]
    (let [threshhold    0.1
          f             (if (p/prime? (first coll)) inc +)
          ratio         (/ prime-count total-count 1.0)
          side-len      (inc (/ (dec total-count) 2))
          ]
    (if (and 
            (> total-count 100)
            (>= threshhold ratio) 
            (zero? (rem (dec total-count) 4)))
        side-len
        (recur (f prime-count) (inc total-count) (rest coll))
        )))

(defn p58 []
    (solve 0 1 (rest (spiral-diagonals)))
    )
