(ns euler.p22)

(defn p22 []
	(let [sorted-names (-> (slurp "http://projecteuler.net/project/names.txt") 
			(clojure.string/split #"\"*,*\"") 	;;;")
			sort vec)
		scores (->> sorted-names
			(map #(map (fn [s] (- (int (char s)) (dec (int \A)))) %))
			(map #(reduce + %)))]
		(reduce + (map * scores (range)))
		))
