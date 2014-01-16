(ns euler.p52
	(:require [euler.fns :as f]))

(defn same-digits?
	[coll]
	"True if all numbers in coll are permutations of the same set of digits"
	(apply = (map #(frequencies (f/digits %)) coll)))
	
(defn p52 []
	(let [start 100000]	; must be a 6+ digit number
	(->>
		(for [n (iterate inc start)] (map #(* n %) (range 1 7)))
		(filter same-digits?)
		first
		first)))
