(ns euler.p30
	(:require [euler.fns :as f]))

(defn p30 []
	(letfn [(sum5p [n] (reduce + (map #(reduce * (repeat 5 %)) (f/digits n))))]
		(->> (range 2 999999)	; TODO smarter way to find upper limit
			(filter #(= % (sum5p %)))
			(reduce +))))
