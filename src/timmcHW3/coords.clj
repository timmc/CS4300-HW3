(ns timmcHW3.coords
   "Coordinates and transformations."
   (:import [java.awt.geom Point2D$Double]))

;-- Conventions --;

;;; World locations and offsets are in homogeneous coordinates.
;;; Viewport locations and offsets are Point2D
;;; Matrices are all 3x3 and represented as a column vector of 3 row vectors.
;;; All vectors are 3-element and are 1-level Clojure vectors.

;-- Matrices --;

(defn- mat3xm-folder
   "Multiply exactly 0 or 2 3x3 matrices."
   ([] [[1 0 0][0 1 0][0 0 1]])
   ([a b]
    (let [[[a00 a01 a02]
           [a10 a11 a12]
           [a20 a21 a22]] a
          [[b00 b01 b02]
           [b10 b11 b12]
           [b20 b21 b22]] b]
       [[(+ (* a00 b00) (* a01 b10) (* a02 b20))
         (+ (* a00 b01) (* a01 b11) (* a02 b21))
         (+ (* a00 b02) (* a01 b12) (* a02 b22))]
        [(+ (* a10 b00) (* a11 b10) (* a12 b20))
         (+ (* a10 b01) (* a11 b11) (* a12 b21))
         (+ (* a10 b02) (* a11 b12) (* a12 b22))]
        [(+ (* a20 b00) (* a21 b10) (* a22 b20))
         (+ (* a20 b01) (* a21 b11) (* a22 b21))
         (+ (* a20 b02) (* a21 b12) (* a22 b22))]])))

(defn mat3xm
   "Multiply zero or more 3x3 matrices."
   [& ms]
   (reduce mat3xm-folder ms))

(defn mat3xv
   "Multiply a 3x3 matrix by a column vector."
   [a v]
   (let [[[a00 a01 a02]
          [a10 a11 a12]
          [a20 a21 a22]] a
         [v0
          v1
          v2] v]
      [(+ (* a00 v0) (* a01 v1) (* a02 v2))
       (+ (* a10 v0) (* a11 v1) (* a12 v2))
       (+ (* a20 v0) (* a21 v1) (* a22 v2))]))

;-- Construction --;

(defn mk-loc
   "Make a 2D point from x and y coordinates."
   [x y]
   [x y 1])

(defn mk-offset
   "Make a 2D offset from x/y vector."
   [x y]
   [x y 0])

(defn crd-x
   "x component of a coordinate"
   [c]
   (c 0))

(defn crd-y
   "y component of a coordinate"
   [c]
   (c 1))

;-- Transformations --;

(defn translator
   "Produce a transformation matrix for moving the origin by (x, y)."
   [x y]
   [[1 0 x]
    [0 1 y]
    [0 0 1]])

(defn rotator
   "Produce a transformation matrix for rotating CCW about the origin by phi radians."
   [phi]
   (let [cp (Math/cos phi)
         sp (Math/sin phi)]
      [[cp (- sp) 0]
       [sp cp 0]
       [0 0 1]]))

(defn scalor
   "Produce a transformation matrix for scaling from the origin."
   ([sx sy]
    [[sx 0  0]
     [0  sy 0]
     [0  0  1]])
   ([s]
    (scalor s s)))
    

;-- Conversion --;

(defn ^Point2D$Double world-to-view
   "Transform world coordinates to view coordinates, dropping loc/offset distinction."
   [wc tmat]
   (let [[vx vy _] (mat3xv tmat wc)]
      (Point2D$Double. vx vy)))

(defn view-to-world
   "Transform view coordinates (location or offset) to world coordinates."
   [^Point2D$Double vc, tmat, is-offset]
   (let [x (. vc x)
         y (. vc y)
         p (if is-offset 0 1)]
      (mat3xv tmat [x y p])))

