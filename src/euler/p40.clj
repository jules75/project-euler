(ns euler.p40
	(:require [euler.fns :as f]
		[clojure.math.combinatorics :as c]))

(defn p40 []
	(let [ch (apply str (range 200000))
		d (map #(reduce *' (repeat % 10)) (range 7))]
		(->>(map #(->> % (nth ch) str Integer/parseInt) d)
			f/undigits f/digits (apply *))))

(defn p41 []	
	(let [len 7	; all 8 or 9 digit pandigitals are divisible by 9
		pandigitals (->> (range 1 (inc len)) c/permutations reverse (map f/undigits))]
		(first (filter f/prime? pandigitals))))