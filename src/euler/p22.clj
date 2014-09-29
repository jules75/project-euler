(ns euler.p22)

(defn p22 []
	(let [url "https://projecteuler.net/project/resources/p022_names.txt"
             sorted-names (-> (slurp url) 
			(clojure.string/split #"\"*,*\"")
			sort vec)
		scores (->> sorted-names
			(map #(map (fn [s] (- (int (char s)) (dec (int \A)))) %))
			(map #(reduce + %)))]
		(reduce + (map * scores (range)))
		))
