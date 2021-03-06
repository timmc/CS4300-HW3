(ns timmcHW3.drawing
  "Drawing on a canvas."
  (:use timmcHW3.utils)
  (:import
   [java.awt Dimension Graphics2D RenderingHints Color BasicStroke]
   [java.awt.geom AffineTransform Path2D Path2D$Double Point2D Point2D$Double
    Line2D Line2D$Double Rectangle2D$Double Ellipse2D Ellipse2D$Double]))

;; TODO: Use binary subdivision to pick nseg better.
(defn ^Path2D de-casteljau
  "Use De Casteljau's algorithm to approximate a Bézier curve
   (given as control Point2Ds) by line segments."
  [points nseg]
  (let [path (Path2D$Double.)
	[xs ys] (apply map vector (map de-pt points))
	[ix0 iy0] (map first [xs ys])]
    (.moveTo path ix0 iy0)
    (dotimes [i (inc nseg)]
      (let [t (/ i nseg)
	    tco (- 1 t)
	    lin #(+ (* %1 tco) (* %2 t))]
	(.lineTo path (interpolate lin xs) (interpolate lin ys))))
    path))

(def ^Color control-sement-color Color/YELLOW)
(def ^BasicStroke control-segment-stroke
  (BasicStroke. (float 2.5) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))

(defn draw-control-segments
  "Draw segments of a control polygon. Specified in view coordinates."
  [^Graphics2D g, vpoints]
  (loop [^Point2D prev (first vpoints)
	 remain (next vpoints)]
    (when remain
      (let [cur (first remain)]
	(.setColor g Color/BLUE)
	(.setStroke g control-segment-stroke)
	(.draw g (Line2D$Double. prev cur))
	(recur cur (next remain))))))

(defn- draw-dot
  "Draw a dot at the specified view coordinates using given color and radius."
  [^Graphics2D g, ^Color c, ^Integer radius, ^Point2D vp]
  (let [[vx vy] (de-pt vp)
        dia (* 2 radius)]
    (doto g
      (.setColor c)
      (.fill (Ellipse2D$Double. (- vx radius) (- vy radius) dia dia)))))

(def ^Color vertex-color Color/GREEN)
(def ^int vertex-radius 3)
(def ^Color hover-color Color/GREEN)
(def ^int hover-radius 5)

(defn draw-control-points
  "Draw vertices of a control polygon. Specified in view coordinates."
  [^Graphics2D g, vpoints]
  (dorun (map (partial draw-dot g vertex-color vertex-radius) vpoints)))

(defn draw-hover
  "Draw the currently hovered vertex (in view coords) of a control polygon."
  [^Graphics2D g, ^Point2D vp]
  (draw-dot g hover-color hover-radius vp))
