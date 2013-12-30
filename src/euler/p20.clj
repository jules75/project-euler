(ns euler.p20 
	(:require [euler.fns :as f]))
	
(defn p20 []
	(f/sum-digits (f/factorial 100)))

(defn p21 []
	(apply +
	(filter #(not (nil? %))
	(for [n (range 10000)]
		(let [m (->> n f/proper-divisors (reduce +))]
			(if (and (not= n m) (= n (->> m f/proper-divisors (reduce +))))
				n)
				)))))
