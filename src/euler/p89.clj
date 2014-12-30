(ns euler.p89
  (:require [clojure.string :as s]))


(defn replace-all
  "Like clojure.string/replace, but with multiple matches/replacements."
  [s matches replacements]
  (if-let [match (first matches)]
	(recur
	 (s/replace s match (first replacements))
	 (rest matches)
	 (rest replacements))
	s))


(defn roman->decimal
  "Returns decimal value of roman numeral string."
  [s]
  (let [subs1 ["IV" "IX" "XL" "XC" "CD" "CM"]
		subs2 ["IIII" "VIIII" "XXXX" "LXXXX" "CCCC" "DCCCC"]
		romans {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
	(->>
	 (replace-all s subs1 subs2)
	 (map #(get romans %))
	 (apply +))))


(defn decimal->roman
  "Returns roman numeral string of given number."
  [n]
  (let [units ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
		tens ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
		hundreds ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]]
	(str
	 (apply str (repeat (int (/ n 1000)) "M"))
	 (get hundreds (mod (int (/ n 100)) 10))
	 (get tens (mod (int (/ n 10)) 10))
	 (get units (mod n 10))
	 )))


(defn p89
  []
  (let [raw (slurp "https://projecteuler.net/project/resources/p089_roman.txt")
		before (count (re-seq #"[IVXLCDM]" raw))
		after (->> raw s/split-lines
				   (map (comp decimal->roman roman->decimal))
				   (apply str)
				   count)]
	(- before after)))

