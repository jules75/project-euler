(ns euler.p20 
	(:require [euler.fns :as f]))
	
(defn p20 []
	(f/sum-digits (f/factorial 100)))
