(ns euler.p35
	(:require [euler.fns :as f]
		[euler.prime :as p]
		[clojure.set :as s]))

(defn rotations 
	[s] 
	"Returns all rotations of string, e.g. abc => bca => cab"
	(let [f #(str (apply str (rest %)) (first %))]
		(take (count s) (iterate f s))))
		
(defn prime-rotations?
	[n]
	(every? p/prime? (map #(Long/parseLong %) (rotations (str n)))))
	
(defn digits-1379?
	[n]
	(empty? (s/difference (set (f/digits n)) #{1 3 7 9})))

(defn p35 [] 
	(->> (p/primes 1000000)
		(filter digits-1379?)
		(filter prime-rotations?)
		count
		(+ 2)	; include 2 and 5 (were filtered out earlier)
		))
