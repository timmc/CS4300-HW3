(ns timmcHW3.utils
  "General utility functions."
  (:import
   [java.awt Dimension]
   [java.awt.geom Point2D Point2D$Double]))

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

;;;-- Mutation --;;;

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

;;;-- Data munging --;;;

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

;;;-- Function munging --;;;

(defn isomorph
  "Return a function that operates within a different space.
   Given f:x->x, mapper:x->y, and inverter:y->x, produces g:y->y."
  [f mapper inverter]
  (fn g [y] (inverter (f (mapper y)))))

;;;-- Geometry --;;;

(defn de-dim
  "Read a Dimension object into a 2-vector of width, height."
  [^Dimension d]
  [(.width d) (.height d)])

(defn ^Point2D pt
  "Make a Point2D object from x and y coordinates."
  [x y]
  (Point2D$Double. x y))

(defn de-pt
  "Read a Point2D object into a 2-vector of x, y."
  [^Point2D p]
  [(.getX p) (.getY p)])

(defrecord ^{:doc "Vector: (x,y) coordinates."}
    Vec2
  [
   ^{:tag double} x
   ^{:tag double} y
   ])

(defn ^Vec2 vec2
  "Create a 2-D Vector."
  [x y]
  (Vec2. x y))

(defn de-vec
  "Read a Vec2 into a 2-vector of x, y."
  [^Vec2 v]
  [(.x v) (.y v)])

(defn ^Vec2 vec+
  "Sum two Vectors."
  [^Vec2 v1, ^Vec2 v2]
  (vec2 (+ (.x v1) (.x v2))
        (+ (.y v1) (.y v2))))

(defn ^Vec2 vec-neg
  "Reverse a Vector."
  [^Vec2 v]
  (vec2 (- (.x v)) (- (.y v))))

(defn ^Vec2 pt-diff
  "Computes vector (- end start)"
  [^Point2D start, ^Point2D end]
  (vec2 (- (.getX end) (.getX start))
        (- (.getY end) (.getY start))))

(defn pt+
  "Add a vector to a point."
  [^Point2D p, ^Vec2 offset]
  (pt (+ (.getX p) (.x offset))
      (+ (.getY p) (.y offset))))

(defn fvec2<pt
  "Transform f:pt->pt (and optional args) into g:vec2->vec2."
  [f & args]
  (isomorph #(apply f % args)
            #(apply pt (de-vec %))
            #(apply vec2 (de-pt %))))
