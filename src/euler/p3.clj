(ns euler.p3
	(:require [euler.fns :as f]))
	
(defn p3 []
	(apply max (f/factor 600851475143)))

