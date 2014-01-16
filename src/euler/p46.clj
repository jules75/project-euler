(ns euler.p46
	(:require [euler.prime :as p]
		[clojure.set :as set]))

(defn twice-squares
	[]
	"Infinite series of 2n^2"
	(map #(* 2 % %) (range)))

(defn sum-primes-and-twice-squares
	[max]
	(for [p (p/primes max) 
			t (take-while #(< % max) (twice-squares))] 
			(+ p t)))
			
(defn p46 []
	(let [limit 10000
		sums (set (sum-primes-and-twice-squares limit))
		odds (set (take limit (filter odd? (range))))]
		(second (sort (set/difference odds sums)))
		))
