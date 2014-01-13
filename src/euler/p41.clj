(ns euler.p41
	(:require [euler.fns :as f]
		[clojure.math.combinatorics :as c]))

(defn p41 []	
	(let [len 7	; all 8 or 9 digit pandigitals are divisible by 9
		pandigitals (->> (range 1 (inc len)) c/permutations reverse (map f/undigits))]
		(first (filter f/prime? pandigitals))))
