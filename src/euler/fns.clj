(ns euler.fns)

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
	(= 1 (count (factor n))))
	
(defn primes
	[]
	"Returns infinite sequence of primes"
	(cons 2 (filter prime? (iterate #(+ 2 %) 3))))
	
(defn triangles
	[]
	"Returns infinite sequence of triangle numbers"
	(->> (map #(/ (* % (dec %)) 2) (range)) rest rest))

(defn count-divisors
	[n]
	"Returns number of divisors for n, uses factor counting method"
	(->> (factor n) frequencies (map last) (map inc) (apply *)))
	
(defn collatz
	[n]
	"Returns Collatz sequence starting with given integer"
	(conj
	(->>
		(iterate #(if (even? %) (/ % 2) (inc (* 3 %))) n)
		(take-while #(not= 1 %))
		vec) 1))

(defn sum-digits
	[n]
	"Sum of all digits in n"
	(->>
		(str n)
		(map #(- (int %) (int \0)))
		(apply +')))

(defn factorial
	[n]
	(reduce *' (range 1 (inc n))))

(defn combinations
	[n r]
	"See calculatorsoup.com/calculators/discretemathematics/combinations.php"
	(/ (factorial n) (*' (factorial r) (factorial (- n r)))))
