(ns euler.p98
  (:require
   [clojure.string :as s]
   [clojure.set :as set]
   [clojure.pprint :refer [pprint]]
   [clojure.math.combinatorics :as c]
   ))


(defn fetch-words
  []
  (let [url "https://projecteuler.net/project/resources/p098_words.txt"
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


(defn all-pairs
  "Returns all possible pairs of items, including in reverse, from collection."
  [coll]
  (mapcat (juxt identity reverse) (c/combinations coll 2)))


(defn anagram?
  "True if s1 and s2 are anagrams."
  [s1 s2]
  (= (sort s1) (sort s2)))


(defn candidate?
  "Returns true if;
  - a and b are anagrams
  - c and d are anagrams
  - a and c correspond
  - b and d correspond
  - a/c correspondence equals b/d correspondence (e.g. ACT/529 = CAT/259)"
  [a b c d]
  (and (anagram? a b)
	   (anagram? c d)
	   (correspond? a c)
	   (correspond? b d)
	   (= (zipmap a c) (zipmap b d))))


(defn p98
  []
  (let [f #(reverse (sort-by (comp count first) (anagram-groups %)))
		word-pairs (mapcat all-pairs (f (fetch-words)))
		num-pairs (mapcat all-pairs (f (str-squares)))]
	(->>
	 (for [[a b] word-pairs
		   [c d] num-pairs
		   :when (candidate? a b c d)]
	   [a b c d])
	 (mapcat #(drop 2 %))
	 set
	 (map #(Integer/parseInt %))
	 sort
	 reverse
	 first
	 )))

