(ns euler.p19)

(defn p19 []
	(let [daynames (-> ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"] repeat flatten)
		months [31 28 31 30 31 30 31 31 30 31 30 31]
		leap-months (assoc months 1 29)
		years (range 1900 2001)
		days (for [year years
			month (if (pos? (mod year 4)) months leap-months)
			daynum (range 1 (inc month))]
			[daynum month year])]
		(->>
			(map #(conj % %2) days daynames)
			(filter #(and (= 1 (first %)) (= "Sun" (last %)) (< 1900 (nth % 2))))
			count)))
