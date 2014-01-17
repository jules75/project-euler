(ns euler.p57
	(:require [euler.fns :as f]))
	
(defn p57 []
	(->>
		(iterate #(/ 1 (+ 2 %)) 0)
		(take 1000)
		rest	; avoid 1
		(map inc)
		(filter #(> (-> % numerator f/digits count)
					(-> % denominator f/digits count)))
		count))
