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
	"Returns infinite sequence of triangle numbers, i.e. 0 1 3 6 10 15 etc."
	(->> (map #(/ (* % (dec %)) 2) (range)) rest))
	
(defn untriangle
	[n]
	"Returns reverse of triangle number, i.e.
		1->2 3->3 6->4 10->5 15->6 etc.
	If result is whole, n is a triangle number"
	(->> n (* 8) inc (Math/sqrt) dec (* 0.5)))

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

(defn digits
	[n]
	"Sequence of digits in integer n"
	(map #(- (int %) (int \0)) (str n)))
		
(defn sum-digits
	[n]
	"Sum of all digits in n"
	(apply +' (digits n)))

(defn factorial
	[n]
	(reduce *' (range 1 (inc n))))

(defn combinations
	[n r]
	"See calculatorsoup.com/calculators/discretemathematics/combinations.php"
	(/ (factorial n) (*' (factorial r) (factorial (- n r)))))
	
(defn as-words
	[n]
	"Return integer as words, supports 1 to 1000 inclusive"
	(let [units [nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
		tens [nil "ten" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"]
		teens [nil "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"]
		rdigits (reverse (digits n))
		n-thou (get units (nth rdigits 3 0))
		n-hund (get units (rem (nth rdigits 2 0) 10))
		n-tens (get tens (rem (nth rdigits 1 0) 10))
		n-unit (get units (rem (nth rdigits 0 0) 10))
		n-teen (get teens (if (< 10 (rem n 100) 20) (- (rem n 100) 10)) nil)]
		(-> (str
			(when n-thou (str n-thou " thousand "))
			(when n-hund (str n-hund " hundred "))
			(when (and n-hund (or n-tens n-unit)) "and ")
			(if n-teen n-teen (str n-tens " " n-unit)))
		clojure.string/trim
		(clojure.string/replace "  " " "))))

(defn walk-tritree
	[tritree path]
	"Walk path from top of triangle tree (see http://projecteuler.net/problem=18)
	Triangle tree is represented as single depth vector
	Path: 0=left 1=right, e.g. [0 0 0 1 1 0 1 0 1 0 1]
	Returns vector of values"
	(->> (let [r (range (count tritree))
		child-map (->> r								; number every node
			(map #(->> (untriangle %) int inc (+ %)))	; find each child index
			(map #(vector % (inc %)))					; and its neighbour
			(zipmap r))]								; build map
		(loop [p path visited [0]]
			(let [child (get-in child-map [(last visited) (first p)])]
				(if (seq p) (recur (rest p) (conj visited child)) visited)
				)))
				(map #(get tritree %))))
