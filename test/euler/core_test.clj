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
		[euler.p29 :refer :all]))

#_(deftest p1-9
	(println "\nProblems 1 to 9")
	(testing
		(let [fns [p1 p2 p3 p4 p5 p6 p7 p8 p9]
			answers [233168 4613732 6857 906609 232792560 25164150
				104743 40824 31875000]]
		(doall (map #(is (= %1 (time (%2)))) answers fns))
		)))

#_(deftest p10-19
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