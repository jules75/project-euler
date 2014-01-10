(ns euler.p5
	(:require [euler.fns :as f]))
		
(defn p5 []
	(->> (map #(frequencies (f/factor %)) (range 2 21))
		(reduce #(merge-with max % %2))
		(map #(repeat (val %) (key %)))
		flatten (apply *')))
