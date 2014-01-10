(ns euler.p10 
	(:require [euler.fns :as f]))

(defn p12 []
	(first (filter #(< 500 (f/count-divisors %)) (f/triangles))))
