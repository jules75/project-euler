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
		[euler.p9 :refer :all]))

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
