(ns euler.fns)

(defn any-divisible?
	[n coll]
	"True if n is evenly divisible by ANY integer in coll"
	(some zero? (map #(rem n %) coll)))

(defmulti palindrome? class)
	
(defmethod palindrome? Long
	[n]
	"True if integer n is a palindrome"
	(= n (->> n str reverse (apply str) Integer/parseInt)))
	
(defmethod palindrome? String
	[s]
	"True if string s is a palindrome"
	(= s (->> s reverse (apply str))))
	
(defn fibonacci
	[]
	"Returns infinite terms of fibonacci series"
	(->> [0 1]
		(iterate #(vector (last %) (+' (first %) (last %))))
		(map first)))
		
(defn factor	; TODO improve performance
	([n m factors]
	"Recursively test is m is factor of n, add to list of factors"
	(if (> m (Math/sqrt n))
		(conj factors n)
		(if (zero? (rem n m))
			(recur (/ n m) m (conj factors m))
			(recur n (if (= 2 m) 3 (+ 2 m)) factors)
		)))
	([n]
	"Return list of factors of n"
	(factor n 2 [])))

(defn sq
	[n]
	"Square n"
	(* n n))

(defn pythagorean?
	[a b c]
	"True if aa+bb=cc and a<b<c"
	(and (< a b c) (= (sq c) (+ (sq a) (sq b)))))
	
(defn prime?
	[n]
	"True if integer is prime"
	(if (> 2 n) false
		(= 1 (count (factor n)))))
	
(defn primes
	[]
	"Returns infinite sequence of primes"
	(cons 2 (filter prime? (iterate #(+ 2 %) 3))))

(defn triangles
	[]
	"Returns infinite sequence of triangle numbers, i.e. 0 1 3 6 10 15 etc."
	(->> (map #(/ (* % (dec %)) 2) (range)) rest))
	
(defn pentagonals
	[]
	"Returns infinite sequence of pentagonal numbers, i.e. 0 1 5 12 22 35 etc."
	(map #(-> % (* 3) dec (* %) (/ 2)) (range)))
	
(defn untriangle	; TODO can we replace with triangle? predicate fn?
	[n]
	"Returns reverse of triangle number, i.e.
		1->2 3->3 6->4 10->5 15->6 etc.
	If result is whole, n is a triangle number"
	(->> n (* 8) inc (Math/sqrt) dec (* 0.5)))
	
(defn triangle?
	[n]
	"True if n is triangle number"
	(-> n (* 8) inc (Math/sqrt) dec (/ 2) (rem 1) zero?))
	
(defn pentagonal? 
	[n] 
	"True if n is pentagonal number"
	(-> n (* 24) inc (Math/sqrt) inc (/ 6) (rem 1) zero?))
	
(defn hexagonal?
	[n]
	"True if n is hexagonal number"
	(-> n (* 8) inc (Math/sqrt) inc (/ 4) (rem 1) zero?))

(defn proper-divisors
	[n]
	"Returns set of proper divisors of n, e.g. 220 -> 1 2 4 5 10 11 20 22 44 55 110"
	(if (zero? n) []
	(->> (range 2 (->> (Math/sqrt n) int inc))
		(filter #(zero? (rem n %)))
		(map #(vector % (/ n %)))
		flatten
		set
		(cons 1))))	

(defn count-divisors
	[n]
	"Returns number of divisors for n, uses factor counting method"
	(->> (factor n) frequencies (map last) (map inc) (apply *)))

(defn digits
	[n]
	"Sequence of digits in integer n"
	(map #(- (int %) (int \0)) (str n)))
	
(defn undigits 
	[coll] 
	"Turn digits into long, e.g. [1 2 5 8 2] => 12582"
	(if (seq coll) (Long/parseLong (apply str coll)) 0))

(defn sum-digits
	[n]
	"Sum of all digits in n"
	(apply +' (digits n)))

(defn factorial
	[n]
	(reduce *' (range 1 (inc n))))
