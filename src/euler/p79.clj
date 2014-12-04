(ns euler.p79
  (:require [clojure.string :as s]
			[clojure.math.combinatorics :as c]))


(def codes (-> "https://projecteuler.net/project/resources/p079_keylog.txt" slurp s/split-lines))


(defn inside?
  "True if every character of s2 occurs IN ORDER within s1."
  [s1 s2]
  (let [regex (re-pattern (apply str (interpose ".*" s2)))]
	(boolean (re-find regex s1))
	))


(defn p79
  "Use brute force search for this. There are only 8 digits, none repeated
  within single 3 digit group."
  []
  (first
   (for [perm (c/permutations "01236789")
		 :let [f #(inside? (apply str perm) %)]
		 :when (every? true? (map f codes))]
	 (Integer/parseInt (apply str perm))
	 )))


;(time (p79))
