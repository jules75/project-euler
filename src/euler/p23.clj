(ns euler.p23
	(:require [euler.fns :as f]
		[clojure.set :as set]))

(defn p23 []
	(let [limit 28123
		abundants (filter #(< % (reduce + (f/proper-divisors %))) (range limit))
		ab-sums (set (for [a abundants b abundants :when (<= a b)] (+ a b)))]
		(->> (set/difference (set (range limit)) ab-sums) (reduce +))
		))
