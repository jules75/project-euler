(ns euler.p30
	(:require [euler.fns :as f]
		[clojure.set :as set]
		[clojure.math.combinatorics :as c]))

(defn p30 []
	(letfn [(sum5p [n] (reduce + (map #(reduce * (repeat 5 %)) (f/digits n))))]
		(->> (range 2 999999)	; TODO smart way to find upper limit
			(filter #(= % (sum5p %)))
			(reduce +))))

(defn p31 []	; TODO doesn't work
	;(let [coins [1 2 5 10 20]]
		(->>
			(for [e (range 10) d (range 20) c (range 40) b (range 100) a (range 200)
				:when (= 200 (+ (* a 1) (* b 2) (* c 5) (* d 10) (* e 20)))]
				[a b c d e])
			count
			time
			))

(defn p32 []
	(reduce +
	(filter pos?
	(for [n (range 8000)]	; TODO smart way to find upper limit
		(if
			(->> (map #(vector % (/ n %) n) (f/proper-divisors n))
				(map #(= (range 1 10) (sort (mapcat f/digits %))))
				(some true?) boolean) n 0
				)))))

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
		(reduce *))))

(defn p34 []
	(letfn [(sum-of-digit-factorials? [n]
		(= n (->> n f/digits (map f/factorial) (reduce +))))]
		(->> (range 3 100000)	; TODO smart way to find upper limit
			(filter sum-of-digit-factorials?)
			(reduce +))))

(defn p35 [] 
	(letfn [(rotations [s] "All rotations of string, e.g. abc => bca => cab"
		(take (count s) (iterate #(str (apply str (rest %)) (first %)) s)))]
	(count (filter
		#(every? (fn [x] (f/prime? (Integer/parseInt x))) (rotations (str %)))
		(range 1000000)))))

(defn p36 []
	(letfn [(base2 [n] "Return n as binary (as string), limited to 2^20"
		(->> (range 20) (map #(bit-and n (bit-shift-left 1 %)))
		(map #(if (pos? %) 1 0)) reverse (drop-while zero?) (apply str)))]
	(->> (range 1000000)
		(filter #(and (f/palindrome? %) (f/palindrome? (base2 %))))
		(reduce +))))
