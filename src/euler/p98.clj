(ns euler.p98
  (:require
   [clojure.string :as s]
   [clojure.math.combinatorics :as c]
   ))


(defn fetch-words
  []
  (let [url "https://projecteuler.net/project/resources/p098_words.txt"
		;url "p098_words.txt"
		raw (slurp url)
		len (count raw)]
	(s/split (subs raw 1 (dec len)) #"\",\"")
	))


(defn anagram-groups
  "For given list of words, return groups of words that are anagrams
  of each other. Ignores words that have no anagram in list."
  [words]
  (->> words
	   (group-by #(apply str (sort %)))
	   (filter #(> (count (last %)) 1))
	   (map last)
	   ))


(defn str-squares
  "Seq of squares (as strings)"
  []
  (let [limit (int (Math/sqrt (Math/pow 10 9)))]
	(map #(str (* % %)) (range limit))
	))


(defn correspond?
  "True if characters in both strings correspond.
  E.g. pipes = sushi = 90914 = event != uluru"
  [s1 s2]
  (when (= (count s1) (count s2))
	(let [pairs (set (map #(str %1 %2) s1 s2))
		  firsts (map first pairs)
		  lasts (map last pairs)]
	  (and (apply distinct? firsts) (apply distinct? lasts))
	  )))


(defn p98
  []
  (let [intify #(map (fn [n] (Integer/parseInt n)) %)
		pairs #(c/combinations % 2)
		f #(mapcat pairs (anagram-groups %))
		word-pairs (f (fetch-words))
		num-pairs (f (str-squares))]
	(reduce max
			(for [[a b] word-pairs
				  [c d] num-pairs
				  :when (correspond? a c)
				  :when (correspond? b d)
				  :when (= (zipmap a c) (zipmap b d))]
			  (apply max (intify [c d]))
			  ))))

;(time (p98))
