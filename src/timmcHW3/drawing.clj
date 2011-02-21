(ns timmcHW3.drawing
  "Drawing on a canvas."
  (:use timmcHW3.utils)
  (:import
   [java.awt Dimension Graphics2D RenderingHints Color BasicStroke]
   [java.awt.geom AffineTransform Path2D Path2D$Double Point2D Point2D$Double
    Line2D Line2D$Double Rectangle2D$Double Ellipse2D Ellipse2D$Double]))

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

(def ^Color vertex-color Color/GREEN)
(def ^Color picked-color Color/WHITE)

(defn draw-control-points
  "Draw vertices of a control polygon. Specified in world coordinates."
  [^Graphics2D g, wpoints, ^AffineTransform to-view, ^Point2D picked]
  (doseq [p wpoints]
    (let [[vcx vcy] (de-pt (.transform to-view p nil))
	  color (if (identical? p picked) picked-color vertex-color)]
      (.setColor g color)
      (.fill g (Ellipse2D$Double. (- vcx 3) (- vcy 3) 6 6)))))

