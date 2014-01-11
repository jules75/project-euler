(ns euler.p24 
	(:require [clojure.math.combinatorics :as c]))

(defn p24 []
	(Long/parseLong (apply str (-> (range 10) c/permutations (nth 999999)))))
