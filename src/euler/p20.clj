(ns euler.p20 
	(:require [euler.fns :as f]
		[clojure.set :as set]
		[clojure.pprint :as pp]
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

(defn p26 [] nil)	; TODO (see git stash)

(defn p27 []
	(->> (for [a (range -1000 1000) b (range -1000 1000)] 
		(vector a b (count (f/quadratic-primes a b))))
		(filter #(< 60 (last %)))
		(sort-by last)
		last 
		butlast 
		(apply *)))

(defn p28 []	; TODO a lot of this code should be in f/spiral
	(let [width 1001
		dirmap {:r [0 1] :d [1 0] :l [0 -1] :u [-1 0]}
		offsets-rel (cons [0 0] (map #(% dirmap) (f/spiral width)))
		offsets-abs (reductions #(map + % %2) offsets-rel)
		centre [(int (/ width 2)) (int (/ width 2))]
		coords (map #(map (fn [n m] (+ n m)) % %2) (repeat centre) offsets-abs)
		diags (concat
			(map #(vector % %2) (range width) (range width))
			(map #(vector % %2) (range width) (reverse (range width))))
		sq-map (zipmap coords (rest (range)))]
		(->> (map #(get sq-map %) diags) distinct (apply +))))
