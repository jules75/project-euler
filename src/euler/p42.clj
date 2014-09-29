(ns euler.p42
	(:require [euler.fns :as f]
		[clojure.string :as s]))

(defn p42 []
	(let [regex #"\",*\"*" ;"
        url "https://projecteuler.net/project/resources/p042_words.txt"
		words (rest (s/split (slurp url) regex))
		score (fn [w] (reduce + (map #(- (int %) (int \A) -1) w)))]
		(->>
			(map #(f/untriangle (score %)) words)
			(filter #(zero? (rem % 1)))
			count)))
