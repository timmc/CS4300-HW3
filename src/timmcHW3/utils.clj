(ns timmcHW3.utils
  "General utility functions."
  (:import
   [java.awt Dimension]
   [java.awt.geom Point2D$Double]))

;;;-- Fixes --;;;

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

;;; Currently works just fine in Clojure, but is here for symmetry.
(defn get-in0
  "A version of get-in that works with an empty collection of keys."
  [m ks]
  (if (seq ks)
    (get-in m ks)
    m))

;;;-- Utility --;;;

(defn rassoc
  "Use assoc-in on the contents of a ref, effectively.
   Evals to true if changed, false otherwise."
  [r ks v]
  (dosync
   (let [old-val (get-in0 @r ks)]
     (if (= v old-val)
       false
       (do
         (ref-set r (assoc-in0 @r ks v))
         true)))))

(defn rupdate
  "Use update-in on the contents of a ref, effectively.
   Evals to true if changed, false otherwise."
  [r ks f & args]
  (dosync
   (let [old-val (get-in0 @r ks)
         new-val (apply f old-val args)]
     (if (= new-val old-val)
       false
       (do
         (ref-set r (assoc-in0 @r ks new-val))
         true)))))

;;; This is done as a macro in order to preserve type hinting.
(defmacro create!
  "Call the function on the restargs, assoc-in the returned value in the ref
   using the keys vector, and return the value."
  [ref-expr ks-expr f-expr & arg-exprs]
  `(let [ref-val# ~ref-expr
	 keys-val# ~ks-expr
	 mk-val# (~f-expr ~@arg-exprs)]
     (dosync
      (ref-set ref-val#
	       (assoc-in0 (deref ref-val#) keys-val# mk-val#)))
     mk-val#))

(defn interpolate-1
  "Perform a single interpolation on a sequence using binary function. Returns
   seq of smaller degree, where each result r_i is (binop d_i d_i+1)."
  [binop vals]
  (map binop (drop-last 1 vals) (drop 1 vals)))

(defn interpolate
  "Perform iterated linear interpolation on a sequence using a binary function.
   Returns final value."
  [binop vals]
  (first (nth (iterate #(interpolate-1 binop %) vals)
	      (dec (count vals)))))

(defn de-dim
  "Read a Dimension object into a 2-vector of width, height."
  [^Dimension d]
  [(.width d) (.height d)])

(defn de-pt
  "Read a Point2D$Double object into a 2-vector of x, y."
  [^Point2D$Double p]
  [(.getX p) (.getY p)])


