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
		[euler.p19 :refer :all]))

(deftest p1-9
	(testing "Problems 1 to 9"
		(is (= 233168 (p1)))
		(is (= 4613732 (p2)))
		(is (= 6857 (p3)))
		(is (= 906609 (p4)))
		(is (= 232792560 (p5)))
		(is (= 25164150 (p6)))
		(is (= 104743 (p7)))
		(is (= 40824 (p8)))
		(is (= 31875000 (p9)))
		))

(deftest p10-19
	(testing "Problems 10 to 19"
		;(is (= 142913828922 (p10)))
		(is (= 70600674 (p11)))
		(is (= 76576500 (p12)))
		(is (= 5537376230 (p13)))
		(is (= 837799 (p14)))			; ~ 15s
		(is (= 137846528820 (p15)))
		(is (= 1366 (p16)))
		(is (= 21124 (p17)))
		(is (= 1074 (p18)))
		(is (= 171 (p19)))
		))
