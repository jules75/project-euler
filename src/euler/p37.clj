(ns euler.p37
	(:require [euler.fns :as f]
			[euler.prime :as p]))

(defn p37 []
	(letfn [(truncate [n] "e.g. 1234 => [1 12 123 1234 1234 234 34 4]"
		(let [d (f/digits n), f #((partial (juxt take take-last) (inc %)) d)]
			(map f/undigits (mapcat f (range (count d))))))]
	(->> (range 11 1000000 2)
		(filter #(every? p/prime? (truncate %)))
		(take 11) ; only 11 truncatable primes with 2+ digits exist
		(reduce +))))
