(ns euler.p33
	(:require [euler.fns :as f]
		[clojure.set :as set]))

(defn p33 []
	(letfn [
		(remove-first [n coll]
			(let [[a b] (map vec (split-with #(not= n %) coll))] (into a (rest b))))
		(shared-digits [n m]
			(set/intersection (set (f/digits n)) (set (f/digits m))))]
	(->>
		(for [a (range 1 100) b (range 1 100) :when (and (< a b) (pos? (rem a 10)))]
			(let [shared (first (shared-digits a b))
				[aa bb] (map #(->> % f/digits (remove-first shared) f/undigits) [a b])]
				(if (and (pos? bb) (not= a aa) (= (/ a b) (/ aa bb)))
					[a b])))
		(remove nil?)
		(map #(/ (first %) (last %)))
		(reduce *)
		(/ 1)
		)))
