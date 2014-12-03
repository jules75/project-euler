(ns euler.p98
  (:require
   [clojure.string :as s]
   [clojure.math.combinatorics :as c]))


(defn fetch-words
  []
  (let [url "https://projecteuler.net/project/resources/p098_words.txt"
		url "p098_words.txt"
		raw (slurp url)
		len (count raw)]
	(s/split (subs raw 1 (dec len)) #"\",\"")))


(defn anagram-groups
  "For given list of words, return groups of words that are anagrams
  of each other. Ignores words that have no anagram in list."
  [words]
  (->> words
	   (group-by #(apply str (sort %)))
	   (filter #(> (count (last %)) 1))
	   (map last)))


(defn str-squares
  "Seq of squares as strings, limit 1 billion."
  []
  (map #(str (* % %)) (range (Math/sqrt 10e8))))


(defn correspond?
  "True if characters in both strings (equal length assumed) correspond.
  E.g. pipes = sushi = 90914 = event != uluru."
  [s1 s2]
  (let [pairs (set (map #(str %1 %2) s1 s2))
		firsts (map first pairs)
		lasts (map last pairs)]
	(and (apply distinct? firsts) (apply distinct? lasts))))


(defn p98
  []
  (let [intify #(map (fn [n] (Integer/parseInt n)) %)
		pairs #(c/combinations % 2)
		f #(mapcat pairs (anagram-groups %))
		word-pairs (f (fetch-words))
		num-pairs (f (str-squares))]
	(reduce max
			(for [[a b] word-pairs
				  [c d] (filter #(= (count a) (count (first %))) num-pairs)
				  :when (= (count a) (count c))
				  :when (= (zipmap a c) (zipmap b d))
				  :when (correspond? a c)
				  :when (correspond? b d)]
			  (apply max (intify [c d]))
			  ))))


;(time (p98))
