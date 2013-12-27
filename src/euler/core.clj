(ns euler.core)

(defn any-divisible?
	[n coll]
	"True if n is evenly divisible by ANY integer in coll"
	(some zero? (map #(rem n %) coll)))
	
(defn palindrome?
	[n]
	"True if integer n is a palindrome"
	(= n (->> n str reverse (apply str) Integer/parseInt)))
	
(defn fibonacci
	[]
	"Return infinite terms of fibonacci series"
	(->> [0 1]
		(iterate #(vector (last %) (+' (first %) (last %))))
		(map first)))
		
(defn factor
	([n m factors]
	"Recursively test is m is factor of n, add to list of factors"
	(if (> m (Math/sqrt n))
		(conj factors n)
		(if (zero? (rem n m))
			(recur (/ n m) m (conj factors m))
			(recur n (inc m) factors)	; TODO improve performance
		)))
	([n]
	"Return list of factors of n"
	(factor n 2 [])))


;; solutions

(defn p1 []
	(reduce + 
		(filter #(any-divisible? % [3 5]) (range 1000))))

(defn p2 []
	(->> (take-while #(< % 4000000) (fibonacci))
		(filter even?)
		(apply +)))
		
(defn p3 []
	(apply max (factor 600851475143)))
	
(defn p4 []
	(->> (for [a (range 1 1000) b (range 1 1000)] (* a b))
		(filter palindrome?)
		(apply max)))
		
(defn p5 []
	(->> (map #(frequencies (factor %)) (range 2 21))
		(reduce #(merge-with max % %2))
		(map #(repeat (val %) (key %)))
		flatten (apply *')))
