(ns timmcHW3.test.coords
   (:use [timmcHW3.coords] :reload)
   (:use [clojure.test]))

;-- inputs --;

(def mat_ident_mult
   [[1 0 0]
    [0 1 0]
    [0 0 1]])

(def mat_a
   [[1 2 3]
    [4 5 6]
    [7 8 9]])

1 2 3
4 5 6
7 8 9

(def mat_b
   [[-1 5 12]
    [0 3 -2]
    [5 11 6]])

-1 5 12
0 3 -2
5 11 6

(def mat_c
   [[0 4 5]
    [2 -3 6]
    [1 10 -17]])

0 4 5
2 -3 6
1 10 -17

;-- expected --;

(def mmult_ab
   [[14 44 26]
    [26 101 74]
    [38 158 122]])

14 44 26
26 101 74
38 158 122

(def mmult_bc
   [[22 101 -179]
    [4 -29 52]
    [28 47 -11]])

22 101 -179
4 -29 52
28 47 -11

(def mmult_abc
   [[114 184 -108]
    [276 541 -522]
    [438 898 -936]])

114 184 -108
276 541 -522
438 898 -936

;-- tests --;

(deftest mmult-ident
   (is (= (mat3xm) mat_ident_mult)))

(deftest mmult-single
   (is (= (mat3xm mat_a) mat_a)))

(deftest mmult-basic ; relies on no widening to double
   (is (= (mat3xm mat_a mat_b) mmult_ab)))

(deftest mmult-multiple
   (is (= (mat3xm mat_a mat_b mat_c) mmult_abc)))

(deftest mmult-associative
   (is (= (mat3xm (mat3xm mat_a mat_b) mat_c)
          (mat3xm mat_a (mat3xm mat_b mat_c)))))
