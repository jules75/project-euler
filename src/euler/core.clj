(ns euler.core)

(def N8 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450N)

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
		
(defn p6 []
	(let [r (range 101) 
		a (sq (apply + r))
		b (apply + (map sq r))]
		(bigint (- a b))))
		
(defn p7 []
	(nth (primes) 10000))
	
(defn p8 []
	(apply max
	(for [group (partition 5 1 (str N8))]
		(->> group
			(map #(Integer/parseInt (str %)))
			(apply *)))))
			
(defn p9 []	; TODO very slow
	(let [r (range 1 500)]
		(for [a r b r c r :when (and (pythagorean? a b c) (= 1000 (+ a b c)))]
			(* a b c)
			)))

