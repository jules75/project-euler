(ns euler.core-test
	(:require [clojure.test :refer :all]
		))

; while 'use' is discouraged, it's handy here
(use '(euler p1 p2 p3 p4 p5 p6 p7 p8 p9))
(use '(euler p10 p11 p12 p13 p14 p15 p16 p17 p18 p19))
(use '(euler p20 p21 p22 p23 p24 p25 p26 p27 p28 p29))
(use '(euler p30 p31 p32 p33 p34 p35 p36 p37 p38 p39))
(use '(euler p40 p41 p42 p43 p44 p45 p46 p47 p48 p49))
(use '(euler p50     p52 p53 p54 p55 p56 p57     p59))
(use '(euler         p62                 p67        ))
(use '(euler                                        ))
(use '(euler                     p85                ))
(use '(euler                         p96            ))

(defn f [a b] (is (= a (time (b)))))

(deftest p1-9
	(println "\nProblems 1 to 9")
	(testing
		(let [fns [p1 p2 p3 p4 p5 p6 p7 p8 p9]
			answers [233168 4613732 6857 906609 232792560 25164150
				104743 40824 31875000]]
		(doall (map f answers fns))
		)))

(deftest p10-19
	(println "\nProblems 10 to 19")
	(testing
		(let [fns [p10 p11 p12 p13 p14 p15 p16 p17 p18 p19]
			answers [142913828922 70600674 76576500 5537376230 837799
				137846528820 1366 21124 1074 171]]
		(doall (map f answers fns))
		)))

(deftest p20-29
	(println "\nProblems 20 to 29")
	(testing
		(let [fns [p20 p21 p22 p23 p24 p25 p26 p27 p28 p29]
			answers [648 31626 871198282 4179871 2783915460 4782
				983 -59231 669171001 9183]]
		(doall (map f answers fns))
		)))

(deftest p30-39
	(println "\nProblems 30 to 39")
	(testing
		(let [fns [p30 p31 p32 p33 p34 p35 p36 p37 p38 p39]
			answers [443839 73682 45228 100 40730 55 872187
				748317 932718654 840]]
		(doall (map f answers fns))
		)))

(deftest p40-49
	(println "\nProblems 40 to 49")
	(testing
		(let [fns [p40 p41 p42 p43 p44 p45 p46 p47 p48 p49]
			answers [210 7652413 162 16695334890 5482660
				1533776805 5777 134043 9110846700 296962999629]]
		(doall (map f answers fns))
		)))

(deftest p50-59
	(println "\nProblems 50 to 59")
	(testing
		(let [fns [p50 p52 p53 p54 p55 p56 p57 p59]
			answers [997651 142857 4075 376 249 972 153 107359]]
		(doall (map f answers fns))
		)))

(deftest p60-69
	(println "\nProblems 60 to 69")
	(testing
		(let [fns [p62 p67]
			answers [127035954683 7273]]
		(doall (map f answers fns))
		)))

(deftest p80-89
	(println "\nProblems 80 to 89")
	(testing
		(let [fns [p85]
			answers [2772]]
		(doall (map f answers fns))
		)))

(deftest p90-99
	(println "\nProblems 90 to 99")
	(testing
		(let [fns [p96]
			answers [24702]]
		(doall (map f answers fns))
		)))
