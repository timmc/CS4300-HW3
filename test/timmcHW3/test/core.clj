(ns timmcHW3.test.core
   (:use [timmcHW3.core] :reload)
   (:use [timmcHW3.state] :reload)
   (:import [timmcHW3.user-data UserData])
   (:use [clojure.test])
   (:import [java.awt.geom Point2D Point2D$Double]))

(deftest picking
   (let [target (Point2D$Double. 100 100)]
      (is (pick-vertex? (- 100 pick-radius) 100 target))
      (is (pick-vertex? (+ 100 pick-radius) 100 target))
      (is (not (pick-vertex? (- 100 pick-radius 1) 100 target)))
      (is (not (pick-vertex? (+ 100 pick-radius 1) 100 target)))))
