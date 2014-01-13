(ns euler.p30
	(:require [euler.fns :as f]))
		
(def pow5 {	; faster than calculating
	0 0
	1 1
	2 32
	3 243
	4 1024
	5 3125
	6 7776
	7 16807
	8 32768
	9 59049})

(defn sum-digits-5th-power?
	[n]
	"True if each digit to the power of 5 adds up to n"
	(= n (reduce + (map #(get pow5 %) (f/digits n)))))
	
(defn p30 []
	(->> (range 2 200000)	; TODO smarter way to find upper limit
		(filter sum-digits-5th-power?)
		(reduce +)
		))
