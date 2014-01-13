(ns euler.p43
	(:require [euler.fns :as f]
		[clojure.math.combinatorics :as c]))

(defn candidate? 
	[digits]
	(let [primes [2 3 5 7 11 13 17]
		groups (map #(f/undigits (subvec digits % (+ % 3))) (range 1 8))
		remainders (map #(rem % %2) groups primes)]
		(every? zero? remainders)))

(defn p43 []
	(->> (c/permutations (range 10))
		(filter #(even? (nth % 3)))						; 4th digit must be even
		(filter #(or (= 0 (nth % 5)) (= 5 (nth % 5))))	; 6th digit must be 0 or 5
		(filter candidate?)
		(map f/undigits)
		(reduce +)))
