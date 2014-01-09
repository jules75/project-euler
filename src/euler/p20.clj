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
	(let [sorted-names (-> (slurp "http://projecteuler.net/project/names.txt") 
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

(defn p26 []
; 
; To calculate the length of period 1/p where p is prime, check if it is
; evenly divisible into 9, then 99, then 999, etc. When you find one divisible 
; by p, the number of 9s is period length.
;
; See point 9 at http://mathworld.wolfram.com/DecimalExpansion.html
; 
	(let [nines (iterate #(+ 9 (*' 10 %)) 9)
		primes (drop 3 (take-while #(< % 1000) (f/primes)))]
	(->>
		(for [p primes] (-> (filter #(zero? (rem % p)) nines) first str count))
		(zipmap primes)
		(sort-by val)
		last first)))

(defn p27 []
	(->> (for [a (range -1000 1000) b (range -1000 1000)] 
		(vector a b (count (f/quadratic-primes a b))))
		(filter #(< 60 (last %)))
		(sort-by last)
		last 
		butlast 
		(apply *)))

(defn p28 []
	(let [w 1001 r (map #(repeat 4 (+ % %)) (rest (range)))]
	(->> (flatten (cons 1 r))
		(reductions +)
		(take (dec (* 2 w))) 
		(reduce +))))

(defn p29 []
	(->> (for [a (range 2 101) b (range 2 101)] (reduce *' (repeat a b)))
		distinct count))
