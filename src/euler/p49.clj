(ns euler.p49
	(:require [euler.fns :as f]
			[euler.prime :as p]))

(defn candidate? 
	[n]
	(let [v [n (+ n 3330) (+ n 6660)]]
		(and (< 1000 n)
			(apply = (map #(sort (f/digits %)) v))
			(every? p/prime? v))))
		
(defn p49 []
	(let [n (->> (p/primes) 
			(take-while #(< % 3340)) 
			(filter candidate?)
			(remove #(= 1487 %)) first)]
		(Long/parseLong (str n (+ 3330 n) (+ 6660 n)))
		))
