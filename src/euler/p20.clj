(ns euler.p20 
	(:require [euler.fns :as f]
		[clojure.set :as set]
		[clojure.math.combinatorics :as c]))
	
(defn p20 []
	(f/sum-digits (f/factorial 100)))

(defn p21 []
	(apply +
	(filter #(not (nil? %))
	(for [n (range 10000)]
		(let [m (->> n f/proper-divisors (reduce +))]
			(if (and (not= n m) (= n (->> m f/proper-divisors (reduce +))))
				n)
				)))))

(defn p22 []
	(let [sorted-names (-> (slurp "names.txt") 
			(clojure.string/split #"\"*,*\"") 	;;;")
			sort vec)
		scores (->> sorted-names
			(map #(map (fn [s] (- (int (char s)) (dec (int \A)))) %))
			(map #(reduce + %)))]
		(reduce + (map * scores (range)))
		))

(defn p23 []
	(let [limit 28123
		abundants (filter #(< % (reduce + (f/proper-divisors %))) (range limit))
		ab-sums (set (for [a abundants b abundants :when (<= a b)] (+ a b)))]
		(->> (set/difference (set (range limit)) ab-sums) (reduce +))
		))

(defn p24 []
	(apply str (-> (range 10) c/permutations (nth 999999))))

(defn p25 []
	(->> (map vector (range) (f/fibonacci))
		(filter #(= 1000 (count (str (last %)))))
		first first))
