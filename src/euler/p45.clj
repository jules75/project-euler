(ns euler.p45
	(:require [euler.fns :as f]))
			
(defn p45 []
	(nth (filter #(and (f/pentagonal? %) (f/hexagonal? %)) (f/triangles)) 2))
