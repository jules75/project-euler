(ns euler.p10 
	(:require [euler.fns :as f]))

(defn p16 []
	(f/sum-digits (reduce * (repeat 1000 2N))))
