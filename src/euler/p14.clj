(ns euler.p14
	(:require [euler.fns :as f]))
	
(defn collatz-term
	[n]
	(if (even? n) (/ n 2) (inc (* 3 n))))

(defn collatz-length
	([n len known]
	(assert (pos? n))	
	(cond
		(contains? known n)	(+ len (get known n))
		(= 1 n)				(inc len)
		:else 				(recur (collatz-term n) (inc len) known)))
	([n known]
	"Calculate length of collatz chain for n
	known is map of known collatz lengths to speed things up"	
		(collatz-length n 0 known)))
		
(defn collatz-range
	([n m found]
	(if (< n m)
		(recur (inc n) m (conj found {n (collatz-length n found)}))
		found))
	([n m]
	"Return length of all collatz chains from n to m-1
	Keeps track of known collatz lengths for performance"
		(collatz-range n m {})))
		
(defn p14 []
	(->> (collatz-range 1 1000000)
		(reduce #(if (> (val %) (val %2)) % %2))
		first))
