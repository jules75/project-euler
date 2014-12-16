(ns euler.p68
  (:require [euler.fns :as f]
			[clojure.string :as s]
			[clojure.math.combinatorics :as c]))


(require '[taoensso.timbre.profiling :refer (profile p)])


(defn split-when
  [pred coll]
  (p :split-when
	 (vector
	  (take-while (complement pred) coll)
	  (drop-while (complement pred) coll)
	  )))


(defn to-5-gon
  "Return 5-gon from set of numbers."
  [coll]
  (p :to-5-gon
	 (let [idx [0 1 2 3 2 4 5 4 6 7 6 8 9 8 1]]
	   (partition 3 (map (partial nth coll) idx))
	   )))


(defn min-rotate
  "Given collection of collections of numbers, rotate (not sort)
  the collections until the one with the lowest first value
  is the first collection."
  [coll]
  (p :min-rotate
	 (let [m (apply min (map first coll))
		   chunks (split-when #(= m (first %)) coll)]
	   (apply concat (reverse chunks))
	   )))


(defn sums-equal?
  "True if sums of first n groups in coll are equal.
  Does it longer way to bail out early if needed."
  [coll]
  (p :sums-equal
	 (let [sums (map #(apply + %) coll)
		   m (first sums)]
	   (every? true? (map #(= m %) (rest sums)))
	 )))


(defn external-ten?
  "True if one of the external points on 5-gon is a ten."
  [coll]
  (p :external-ten?
  (some #{10} (map (partial nth coll) [0 3 5 7 9]))))


(defn p68
  []
  (->>
   (c/permutations (range 1 11)) 			; all possible 1-10 permutations
   (filter external-ten?) 					; ignore if ten is internal (makes 17 digits)
   (map to-5-gon) 							; turn into 5-gon structures
   (filter sums-equal?) 					; satisfy 'magic' requirement
   (map min-rotate) 						; satisfy 'lowest external node first'
   (map (comp (partial apply str) flatten)) ; turn into string
   ;(filter #(= 16 (count %))) 				; must be 16 digits
   (map bigint) 							; turn back into numbers
   (reduce max) 							; find biggest
   ))


;(profile :info :Arithmetic (p68))

(time (p68))
