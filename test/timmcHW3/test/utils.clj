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
  (is (= ((isomap inc int char) \3) \4))
  (is (= ((isomap (partial * 2) int char) \space) \@)))

(deftest build
  (is (= (pt 2 3) (Point2D$Double. 2 3)))
  (let [v (Vec2. 4 5)]
    (is (= (.x v) 4)
        (= (.y v) 5))))

(deftest sum
  (is (= (pt+ (pt 2 3) (Vec2. 4 -5)) (pt 6 -2)))
  (is (= (vec+ (Vec2. 2 3) (Vec2. 4 -5)) (Vec2. 6 -2))))

(deftest neg
  (is (= (vec-neg (Vec2. 3 4)) (Vec2. -3 -4))))

(deftest diff
  (is (= (pt-diff (pt 100 130) (pt 102 135)) (Vec2. 2 5))))

(deftest destructuring
  (is (= (de-pt (pt -2 15)) [-2 15]))
  (is (= (de-vec (Vec2. 3 4)) [3 4]))
  (is (= (de-dim (Dimension. 3 12)) [3 12])))

