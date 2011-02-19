(ns timmcHW3.test.utils
   (:use [timmcHW3.utils] :reload)
   (:use [clojure.test])
   (:import [java.awt.geom Point2D$Double]))


(deftest complete-update
   (let [complex {:foo "bar" :qux [4 0]}
         altered {:foo "bar" :qux [4 7]}]
      (is (= (update-in0 complex [:qux 1] + 3 4) altered))
      (is (= (update-in0 complex [] identity) complex))))

(deftest replacing-update
   (let [complex {:foo "bar" :qux [4 0]}
         altered {:foo "bar" :qux [4 7]}]
      (is (= (assoc-in0 complex [:qux 1] 7) altered))))

