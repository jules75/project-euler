(ns euler.p29)

(defn p29 []
	(->> (for [a (range 2 101) b (range 2 101)] (reduce *' (repeat a b)))
		distinct count))
