(ns euler.p68
  (:require [euler.fns :as f]
			[clojure.string :as s]
			[clojure.math.combinatorics :as c]))


(defn split-when
  [pred coll]
  (vector
   (take-while (complement pred) coll)
   (drop-while (complement pred) coll)
   ))


(defn to-5-gon
  "Return 5-gon from set of numbers."
  [coll]
  (let [idx [0 1 2 3 2 4 5 4 6 7 6 8 9 8 1]]
	(partition 3 (map (partial nth coll) idx))
	))


(defn min-rotate
  "Given collection of collections of numbers, rotate (not sort)
  the collections until the one with the lowest first value
  is the first collection."
  [coll]
  (let [m (apply min (map first coll))
		chunks (split-when #(= m (first %)) coll)]
	(apply concat (reverse chunks))
	))


(defn p68
  []
  (->>
   (c/permutations (range 1 11)) 		; all possible 1-10 permutations
   (map to-5-gon) 						; turn into 5-gon structures
   (map min-rotate) 					; satisfy 'lowest external node first'
   (filter #(apply = (map (partial apply +) %))) 	; satisfy 'magic' requirement
   (map (comp (partial apply str) flatten)) 		; turn into string
   (filter #(= 16 (count %))) 			; must be 16 digits
   (map bigint) 						; turn back into numbers
   (reduce max) 						; find biggest
   ))

