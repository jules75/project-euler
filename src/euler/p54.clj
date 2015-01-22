(ns euler.p54
  (:require [clojure.string :as s]))


(defn cardify
  "Turns 2 char string (e.g. AS, 5H) into playing card map."
  [s]
  (let [values (zipmap [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] (range 2 15))
		suits {\H :hearts \D :diamonds \C :clubs \S :spades}]
	{:suit (get suits (last s))
	 :value (get values (first s))
	 :str s}))


(defn fetch-hand-pairs
  "Fetch poker hands from Project Euler."
  []
  (let [url "https://projecteuler.net/project/resources/p054_poker.txt"
		f #(split-at 5 (map cardify (s/split % #"\s")))]
	(map f (s/split-lines (slurp url)))
	))


(defn flush?
  [hand]
  (apply = (map :suit hand)))


(defn straight?
  [hand]
  (let [values (sort (set (map :value hand)))]
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
   (= 14 (->> (map :value hand) sort last))))


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
  "Returns the rank of the hand, the higher the better."
  [hand]
  (cond
   (royal-flush? hand)		10
   (straight-flush? hand)	9
   (n-of-a-kind? hand 4)	8
   (full-house? hand)		7
   (flush? hand)			6
   (straight? hand)			5
   (n-of-a-kind? hand 3)	4
   (two-pairs? hand)		3
   (n-of-a-kind? hand 2)	2
   :else					1))


(defn sort-hand
  "Returns hand sorted by rank then by value."
  [hand]
  (let [freqs (frequencies (map :value hand))]
	(->> hand
		 (map #(assoc % :freq (get freqs (:value %))))	; attach frequency
		 (sort-by (juxt :freq :value))					; sort by rank, then value
		 reverse)))										; highest first


(defn highest?
  "True if hand1 has higher value than hand2.
  Test only reliable if hands are same rank."
  [hand1 hand2]
  (let [f #(first (map :value (sort-hand %)))
		c1 (f hand1)
		c2 (f hand2)]
	(cond
	 (= c1 c2) (recur (rest hand1) (rest hand2))
	 (> c1 c2) true
	 :else false)))


(defn p54 []
  (count
   (filter true?
		   (for [[h1 h2] (fetch-hand-pairs)]
			 (cond
			  (> (rank h1) (rank h2)) true
			  (< (rank h1) (rank h2)) false
			  :else (highest? h1 h2)
			  )))))


;(p54)
