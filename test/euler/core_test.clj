(ns euler.core-test
	(:require [clojure.test :refer :all]
		))

(def solutions {
    :p1    233168
    :p2    4613732
    :p3    6857
    :p4    906609
    :p5    232792560
    :p6    25164150
    :p7    104743
    :p8    40824
    :p9    31875000

    :p10   142913828922
    :p11   70600674
    :p12   76576500
    :p13   5537376230
    :p14   837799
    :p15   137846528820
    :p16   1366
    :p17   21124
    :p18   1074
    :p19   171

    :p20    648
    :p21    31626
    :p22    871198282
    :p23    4179871
    :p24    2783915460
    :p25    4782
    :p26    983
    :p27    -59231
    :p28    669171001
    :p29    9183

    :p30    443839
    :p31    73682
    :p32    45228
    :p33    100
    :p34    40730
    :p35    55
    :p36    872187
    :p37    748317
    :p38    932718654
    :p39    840

    :p40    210
    :p41    7652413
    :p42    162
    :p43    16695334890
    :p44    5482660
    :p45    1533776805
    :p46    5777
    :p47    134043
    :p48    9110846700
    :p49    296962999629

    :p50    997651
    :p52    142857
    :p53    4075
    :p54    376
    :p55    249
    :p56    972
    :p57    153
    :p58    26241
    :p59    107359

    :p62    127035954683
    :p67    7273

    :p75    161667

    :p84    101524
    :p85    2772

	:p92 	8581146
    :p96    24702
    :p97    8739992577
	:p98	18769
                 })

(defn load-and-execute
    "Return result for given problem number, e.g. 'p53'"
    [problem]
    (let [s (str "(do (use 'euler." problem ") (euler." problem "/" problem "))")]
        (eval (read-string s))
        ))

(deftest compare-answers
    (doseq [pair solutions :let [prob (key pair) answer (val pair)]]
        (print prob "\t")
        (is (= answer (time (load-and-execute (name prob)))))
        ))
