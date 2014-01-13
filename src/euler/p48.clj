(ns euler.p48)

(defn p48 []
	(->> (range 1 1001)
		(map #(reduce *' (repeat % %)))
		(reduce +')
		str
		(take-last 10)
		(apply str)
		Long/parseLong))
