(ns euler.p23
	(:require [euler.fns :as f]
		[clojure.set :as set]))

(defn abundant?
	[n]
	"True sum of n's proper divisors exceed n"
	(< n (reduce + (f/proper-divisors n))))
	
(defn sum-pair?
	[n coll]
	"Returns true if any 2 numbers in coll add up to n
	For performance, coll should be map with ints for keys, e.g.
	{0 nil, 13 nil, 99 nil}, values are ignored"
	(boolean (some true?
		(for [a (keys coll) :when (contains? coll (- n a))] true)
		)))

(defn p23 []
	(let [limit 28123
		candidates (into (range 2 limit 2) (range 15 limit 30))	; even or div by 15
		abundants (filter abundant? candidates)
		nmap (zipmap abundants (repeat nil))] 					; for performance
		(reduce + (remove #(sum-pair? % nmap) (range limit)))
		))
