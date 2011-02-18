(ns timmcHW3.utils
   "General utility functions."
   (:import
      [java.awt Dimension]
      [java.awt.geom Point2D$Double]))

;-- Fixes --;

(defn update-in0
   "A version of update-in that works with an empty collection of keys."
   [m ks f & args]
   (if (seq ks)
      (apply update-in m ks f args)
      (apply f m args)))

(defn assoc-in0
   "A version of assoc-in that works with an empty collection of keys."
   [m ks v]
   (if (seq ks)
      (assoc-in m ks v)
      v))

; Currently works just fine in Clojure, but is here for symmetry.
(defn get-in0
   "A version of get-in that works with an empty collection of keys."
   [m ks]
   (if (seq ks)
      (get-in m ks)
      m))

;-- Utility --;

(defn assoc-in-ref!
   "Update an associative ref with a new value given a key vector. Evals to true if changed, false otherwise."
   [aref ks v]
   (let [old-val (get-in0 @aref ks)]
      (if (= v old-val)
         false
         (dosync
            (ref-set aref (assoc-in0 @aref ks v))
            true))))
    

; This is done as a macro in order to preserve type hinting.
(defmacro create!
   "Call the function on the restargs, assoc-in the returned value in the ref using the keys vector, and return the value."
   [ref-expr ks-expr f-expr & arg-exprs]
   `(let [ref-val# ~ref-expr
            keys-val# ~ks-expr
            mk-val# (~f-expr ~@arg-exprs)]
       (dosync
          (ref-set ref-val#
             (assoc-in0 (deref ref-val#) keys-val# mk-val#)))
       mk-val#))

;-- Conventions --;

;;; Coordinates, points, and 2-vectors are represented as [x y] pairs.
;;; All curve data is represented in standard Cartesian world coordinates.

(defn de-dim
   "Read a Dimension object into a 2-vector of width, height."
   [^Dimension d]
   [(.width d) (.height d)])

(defn de-pt
   "Read a Point2D$Double object into a 2-vector of x, y."
   [^Point2D$Double p]
   [(.getX p) (.getY p)])


