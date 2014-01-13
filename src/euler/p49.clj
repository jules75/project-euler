(ns euler.p49
	(:require [euler.fns :as f]))

(defn candidate? 
	[n]
	(let [v [n (+ n 3330) (+ n 6660)]]
		(and (< 1000 n)
			(apply = (map #(sort (f/digits %)) v))
			(every? f/prime? v))))
		
(defn p49 []
	(let [n (->> (f/primes) 
			(take-while #(< % 3340)) 
			(filter candidate?)
			(remove #(= 1487 %)) first)]
		(Long/parseLong (str n (+ 3330 n) (+ 6660 n)))
		))
