(ns euler.p17
	(:require [euler.fns :as f]))

(defn p17 []
	(->
		(->> (range 1 1001) (map f/as-words) (apply str))
		(clojure.string/replace " " "")
		count))
