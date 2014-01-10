(ns euler.p15 
	(:require [euler.fns :as f]))
	
(defn count-combinations
	[n r]
	"See calculatorsoup.com/calculators/discretemathematics/combinations.php"
	(/ (f/factorial n) (*' (f/factorial r) (f/factorial (- n r)))))

(defn p15 []
	(count-combinations 40 20))
