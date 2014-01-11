(ns euler.p28)

(defn p28 []
	(let [w 1001 r (map #(repeat 4 (+ % %)) (rest (range)))]
	(->> (flatten (cons 1 r))
		(reductions +)
		(take (dec (* 2 w))) 
		(reduce +))))
