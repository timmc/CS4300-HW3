(ns timmcHW3.test.core
   (:use [timmcHW3.core] :reload)
   (:use [clojure.test])
   (:import [java.awt.geom Point2D$Double]))


(deftest complete-update
   (let [complex {:foo "bar" :qux [4 0]}
         altered {:foo "bar" :qux [4 7]}]
      (is (= (complete-update-in complex [:qux 1] + 3 4) altered))
      (is (= (complete-update-in complex [] identity) complex))))

(deftest add-point
   (let [blank {:pending-points ()}
         single {:pending-points (list (Point2D$Double. 4 4))}
         full {:pending-points (list (Point2D$Double. 0 0)
                                     (Point2D$Double. 1 1)
                                     (Point2D$Double. 2 2)
                                     (Point2D$Double. 3 3))}]
      (is (= (add-pending-point blank (Point2D$Double. 4 4)) single))
      (is (= (add-pending-point full (Point2D$Double. 4 4)) single))))
