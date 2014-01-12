(ns euler.p30
	(:require [euler.fns :as f]))

(defn sum5p 
	[n] 
	(reduce + (map #(reduce * (repeat 5 %)) (f/digits n))))
	
(defn p30 []
	(->> (range 2 999999)	; TODO smart way to find upper limit
		(filter #(= % (sum5p %)))
		(reduce +)))
