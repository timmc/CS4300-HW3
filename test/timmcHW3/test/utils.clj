(ns timmcHW3.test.utils
   (:use [timmcHW3.utils] :reload)
   (:import [timmcHW3.utils Vec2])
   (:use [clojure.test])
   (:import
    [java.awt Dimension]
    [java.awt.geom Point2D Point2D$Double]))


(deftest complete-update
   (let [complex {:foo "bar" :qux [4 0]}
         altered {:foo "bar" :qux [4 7]}]
      (is (= (update-in0 complex [:qux 1] + 3 4) altered))
      (is (= (update-in0 complex [] identity) complex))))

(deftest replacing-update
   (let [complex {:foo "bar" :qux [4 0]}
         altered {:foo "bar" :qux [4 7]}]
      (is (= (assoc-in0 complex [:qux 1] 7) altered))))

(deftest iso
  (is (= ((isomorph inc int char) \3) \4))
  (is (= ((isomorph (partial * 2) int char) \space) \@)))

(deftest impl
  (is (= (pt 2 3) (Point2D$Double. 2 3)))
  (is (= (vec2 2 3) (Vec2. 2 3))))

(deftest inverse-build
  (is (= (de-pt (pt 2 3)) [2 3]))
  (is (= (de-vec (vec2 2 3)) [2 3])))

(deftest sum
  (is (= (pt+ (pt 2 3) (vec2 4 -5)) (pt 6 -2)))
  (is (= (vec+ (vec2 2 3) (vec2 4 -5)) (vec2 6 -2))))

(deftest neg
  (is (= (vec-neg (vec2 3 4)) (vec2 -3 -4))))

(deftest diff
  (is (= (pt-diff (pt 100 130) (pt 102 135)) (vec2 2 5))))

(deftest destructuring
  (is (= (de-pt (pt -2 15)) [-2 15]))
  (is (= (de-vec (vec2 3 4)) [3 4]))
  (is (= (de-dim (Dimension. 3 12)) [3 12])))

