(ns euler.core-test
	(:require [clojure.test :refer :all]
		[euler.p1 :refer :all]
		[euler.p2 :refer :all]
		[euler.p3 :refer :all]
		[euler.p4 :refer :all]
		[euler.p5 :refer :all]
		[euler.p6 :refer :all]
		[euler.p7 :refer :all]
		[euler.p8 :refer :all]
		[euler.p9 :refer :all]
		[euler.p10 :refer :all]
		[euler.p11 :refer :all]
		[euler.p12 :refer :all]
		[euler.p13 :refer :all]
		[euler.p14 :refer :all]
		[euler.p15 :refer :all]
		[euler.p16 :refer :all]
		[euler.p17 :refer :all]
		[euler.p18 :refer :all]
		[euler.p19 :refer :all]
		[euler.p20 :refer :all]
		[euler.p21 :refer :all]
		[euler.p22 :refer :all]
		[euler.p23 :refer :all]
		[euler.p24 :refer :all]
		[euler.p25 :refer :all]
		[euler.p26 :refer :all]
		[euler.p27 :refer :all]
		[euler.p28 :refer :all]
		[euler.p29 :refer :all]
		[euler.p30 :refer :all]
		[euler.p31 :refer :all]
		[euler.p32 :refer :all]
		[euler.p33 :refer :all]
		[euler.p34 :refer :all]
		[euler.p35 :refer [p35]]
		[euler.p36 :refer :all]
		[euler.p37 :refer :all]
		[euler.p38 :refer :all]
		[euler.p39 :refer :all]
		[euler.p40 :refer :all]
		[euler.p41 :refer :all]
		[euler.p42 :refer :all]
		[euler.p43 :refer [p43]]
		[euler.p44 :refer :all]
		[euler.p45 :refer :all]
		[euler.p46 :refer :all]
		[euler.p47 :refer :all]
		[euler.p48 :refer :all]
		[euler.p49 :refer :all]
		[euler.p50 :refer :all]
		[euler.p52 :refer :all]
		[euler.p53 :refer :all]
		[euler.p54 :refer :all]
		[euler.p55 :refer :all]
		[euler.p56 :refer :all]
		[euler.p57 :refer :all]		
		[euler.p62 :refer :all]
		[euler.p67 :refer :all]
    [euler.p85 :refer :all]
		))

(deftest p1-9
	(println "\nProblems 1 to 9")
	(testing
		(let [fns [p1 p2 p3 p4 p5 p6 p7 p8 p9]
			answers [233168 4613732 6857 906609 232792560 25164150
				104743 40824 31875000]]
		(doall (map #(is (= %1 (time (%2)))) answers fns))
		)))

(deftest p10-19
	(println "\nProblems 10 to 19")
	(testing
		(let [fns [p10 p11 p12 p13 p14 p15 p16 p17 p18 p19]
			answers [142913828922 70600674 76576500 5537376230 837799
				137846528820 1366 21124 1074 171]]
		(doall (map #(is (= %1 (time (%2)))) answers fns))
		)))

(deftest p20-29
	(println "\nProblems 20 to 29")
	(testing
		(let [fns [p20 p21 p22 p23 p24 p25 p26 p27 p28 p29]
			answers [648 31626 871198282 4179871 2783915460 4782
				983 -59231 669171001 9183]]
		(doall (map #(is (= %1 (time (%2)))) answers fns))
		)))

(deftest p30-39
	(println "\nProblems 30 to 39")
	(testing
		(let [fns [p30 p31 p32 p33 p34 p35 p36 p37 p38 p39]
			answers [443839 73682 45228 100 40730 55 872187
				748317 932718654 840]]
		(doall (map #(is (= %1 (time (%2)))) answers fns))
		)))

(deftest p40-49
	(println "\nProblems 40 to 49")
	(testing
		(let [fns [p40 p41 p42 p43 p44 p45 p46 p47 p48 p49]
			answers [210 7652413 162 16695334890 5482660
				1533776805 5777 134043 9110846700 296962999629]]
		(doall (map #(is (= %1 (time (%2)))) answers fns))
		)))

(deftest p50-59
	(println "\nProblems 50 to 59")
	(testing
		(let [fns [p50 p52 p53 p54 p55 p56 p57]
			answers [997651 142857 4075 376 249 972 153]]
		(doall (map #(is (= %1 (time (%2)))) answers fns))
		)))

(deftest p60-69
	(println "\nProblems 60 to 69")
	(testing
		(let [fns [p62 p67]
			answers [127035954683 7273]]
		(doall (map #(is (= %1 (time (%2)))) answers fns))
		)))

(deftest p80-89
	(println "\nProblems 80 to 89")
	(testing
		(let [fns [p85]
			answers [2772]]
		(doall (map #(is (= %1 (time (%2)))) answers fns))
		)))
