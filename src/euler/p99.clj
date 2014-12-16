(ns euler.p99
  (:require
   [clojure.math.numeric-tower :refer [expt sqrt]]
   ))


(defn max-exp
  "Given two pairs of numbers [a b] [c d], returns pair of highest value
  for a^b and c^d. Each pair also has an index (n/m)."
  [[n [a b]] [m [c d]]]
  (let [x (* b (Math/log10 a))
		y (* d (Math/log10 c))]
	(if (> x y) [n [a b]] [m [c d]])))


(defn p99
  []
  (->>
   "https://projecteuler.net/project/resources/p099_base_exp.txt"
   slurp
   (re-seq #"\d+")
   (map #(Integer/parseInt %))
   (partition 2)
   (zipmap (range 1 Double/POSITIVE_INFINITY))
   (reduce max-exp)
   first))

