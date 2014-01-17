(ns euler.p54
	(:require [euler.fns :as f]
			[clojure.pprint :as pp]
			[clojure.math.numeric-tower :as math]
			[clojure.string :as s]))
			
(def values { \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})
	
(def suits { \H :hearts \D :diamonds \C :clubs \S :spades})
			
(defn cardify
	[s]
	"Turns 2 char string (e.g. AS, 5H) into playing card map"
	{:suit (suits (last s))
	:value (values (first s))
	:str s})

(defn fetch-hand-pairs
	[]
	"Fetch poker hands from Project Euler"
	(->>
		(slurp "http://projecteuler.net/project/poker.txt")
		s/split-lines
		(map #(s/split % #"\s"))
		(map #(map cardify %))
		(map #(split-at 5 %))))
		
(defn flush?
	[hand]
	(apply = (map :suit hand)))
	
(defn straight?
	[hand]
	(let [values (distinct (sort (map :value hand)))]
		(and 
			(= 5 (count values))					; 5 different values
			(= 4 (- (last values) (first values)))	; each 1 apart
			)))

(defn straight-flush?
	[hand]
	(and (flush? hand) (straight? hand)))

(defn royal-flush?
	[hand]
	(and 
		(straight-flush? hand)
		(= 14 (->> hand (map :value) sort last))))
	
(defn full-house?
	[hand]
	(= 2 (count (frequencies (map :value hand)))))

(defn n-of-a-kind?
	[hand n]
	(->> (map :value hand)
		frequencies
		(some #(= n (val %)))
		boolean))
		
(defn two-pairs?
	[hand]
	(->> (map :value hand)
		frequencies
		(filter #(= 2 (val %)))
		count
		(= 2)))

(defn rank
	[hand]
	"Returns the rank of the hand, the higher the better"
	(cond
		(royal-flush? hand)		10
		(straight-flush? hand)	9
		(n-of-a-kind? hand 4)	8
		(full-house? hand)		7
		(flush? hand)			6
		(straight? hand)		5
		(n-of-a-kind? hand 3)	4
		(two-pairs? hand)		3
		(n-of-a-kind? hand 2)	2
		:else					1))
		
(defn sort-hand
	[hand]
	"Returns hand sorted by rank then by value"
	(let [freqs (frequencies (map :value hand))]
		(->> hand
			(map #(assoc % :freq (get freqs (:value %))))	; attach frequency
			(sort-by (juxt :freq :value))					; sort by rank, then value
			reverse)))										; highest first

(defn highest?
	[hand1 hand2]
	"True if hand1 has higher value than hand2
	Hands MUST be sorted with sort-hand before being passed
	Test only reliable if hands have same rank"
	(let [h1 (map :value hand1) h2 (map :value hand2)]
		(cond
			(= (first h1) (first h2)) (recur (rest hand1) (rest hand2))
			(> (first h1) (first h2)) true
			:else false)))
			
(defn p54 []
	(count (filter true?
	(for [[h1 h2] (fetch-hand-pairs)]
		(cond
			(> (rank h1) (rank h2)) true
			(< (rank h1) (rank h2)) false
			(= (rank h1) (rank h2)) (highest? (sort-hand h1) (sort-hand h2))
			)))))

