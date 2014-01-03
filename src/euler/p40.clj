(ns euler.p40
	(:require [euler.fns :as f]
		[clojure.string :as s]
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

(defn p42 []
	(let [regex #"\",*\"*" ;"
		words (rest (s/split (slurp "https://projecteuler.net/project/words.txt") regex))
		score (fn [w] (reduce + (map #(- (int %) (int \A) -1) w)))]
		(->>
			(map #(f/untriangle (score %)) words)
			(filter #(zero? (rem % 1)))
			count)))

(defn p43 []
	(letfn [(candidate? [digits]
		(every? true?
			(map #(zero? (rem (f/undigits (subvec digits % (+ % 3))) %2))
				(range 1 8) (f/primes))))]
	(->> (filter candidate? (c/permutations (range 10)))
		(map f/undigits)
		(apply +))))
		
(defn p44 []
	(let [pents (take 10000 (rest (f/pentagonals)))]
		(for [a pents b pents
			:when (< a b)
			:when (f/pentagonal? (- b a))
			:when (f/pentagonal? (+ a b))]
			(- b a))))
			
(defn p45 []
	(nth (filter #(and (f/pentagonal? %) (f/hexagonal? %)) (f/triangles)) 2))
