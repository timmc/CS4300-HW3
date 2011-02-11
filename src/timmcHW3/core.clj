(ns timmcHW3.core
   "Core code. Use -main."
   (:import [javax.swing SwingUtilities UIManager
                         JFrame JComponent JPanel JMenu JMenuBar JMenuItem JButton]
            [java.awt BorderLayout Graphics2D RenderingHints Dimension Color Component]
            [java.awt.geom AffineTransform Path2D Path2D$Double Point2D Point2D$Double Rectangle2D$Double]
            [java.awt.event ActionListener ComponentAdapter])    
   (:gen-class))

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

;-- Constants --;

(def ^{:doc "Multiplier for incremental zoom in."}
   zoom-factor 1.1)

;-- Viewpoint --;

; Translation: User drags new world point to center of window.
(def ^{:doc "Chosen center of rotation."}
   rot-center
   (ref [0 0]))

; Scale: Resizing the window stretches the view.
(def ^{:doc "Minimum extent of world to show in both width and height."}
   view-minspect
   (ref 200))

; Rotation: Around the center of the window.
(def ^{:doc "Rotation of viewport."}
   view-rot (ref 0))

; Centering: This is the center of rotation of the viewpoint.
(def ^{:doc "Viewport's pixel center coordinates as [x y]."}
   view-center
   (ref [0 0]))

(def ^{:doc "World-to-viewport transform." :tag AffineTransform}
   xform-to-view
   (ref (AffineTransform.)))

(def ^{:doc "The inverse transform, viewport-to-world." :tag AffineTransform}
   xform-from-view
   (ref (AffineTransform.)))

(def ^{:doc "Dimensions of viewport." :tag Dimension}
   viewport-dim
   (ref (Dimension. 1 1)))

(defn ^AffineTransform calc-xform
   "Calculate the world-to-viewport transformation."
   [view-w view-h]
   (dosync
      (let [[drag-x drag-y] @rot-center
            minspect (min view-w view-h)
            magnification (/ minspect @view-minspect)]
         (doto (AffineTransform.)
            (.translate (/ view-w 2) (/ view-h 2))
            (.rotate (- @view-rot))
            (.scale 1 -1) ; flip y coords
            (.scale magnification magnification) ; zoom
            (.translate (- drag-x) (- drag-y))))))

(defn update-xform!
   "Update the world-to-viewpoint and inverse transformations."
   []
   (dosync
      (let [[w h] (de-dim @viewport-dim)
            at (calc-xform w h)]
          (ref-set xform-to-view at)
          (ref-set xform-from-view (.createInverse at)))))

;-- Data --;

(def ^{:doc "Control polygon as coords list"} ;TODO better than cubic
   control-polygon (ref (list [-50 0]
                              [0 30]
                              [0 -30]
                              [5 0])))

;-- Math --;

;TODO: add {set,nudge}-{rotation,zoom,translation-{x,y}}! functions
;TODO: add functions to apply transforms to collections of coords

(defn ^Path2D calc-path
   "Calculate a path based on the current control points."
   [] ;TODO accept polyline as arg?
   (let [points @control-polygon
         ^Path2D path (Path2D$Double.)]
      (when (= (count points) 4)
         (let [[p0 p1 p2 p3] points]
            (.moveTo path (p0 0) (p0 1))
            (.curveTo path (p1 0) (p1 1)
                           (p2 0) (p2 1)
                           (p3 0) (p3 1))))
      (.createTransformedShape ^AffineTransform @xform-to-view path)))

(defn ^Point2D loc-to-view
   "Transform a location from world to viewport coords."
   ([wx wy]
    (.transform ^AffineTransform @xform-to-view (Point2D$Double. wx wy) nil))
   ([^Point2D p]
    (.transform ^AffineTransform @xform-to-view p nil)))

;-- Rendering --;

(defn test-draw-point
   "Draw a test point at the given coords."
   [^Graphics2D g, ^Color c, wx, wy]
   (let [[vx vy] (de-pt (loc-to-view wx wy))]
      (doto g
         (.setPaint c)
         (.fill (Rectangle2D$Double. (- vx 3) (- vy 3) 6 6)))))

(defn render
   "Draw the world."
   [^Graphics2D g]
   (let [[w h] (de-dim @viewport-dim)
         [cx cy] (de-pt @view-center)]
      (doto g
         (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
         (.setColor (Color. 50 50 50))
         (.fill (Rectangle2D$Double. 0 0 w h))
         (.setColor Color/YELLOW)
         (.draw (Rectangle2D$Double. cx 0 0.1 h))
         (.draw (Rectangle2D$Double. 0 cy w 0.1))
         (.draw (calc-path))))
   (test-draw-point g Color/WHITE 0 0) ; center
   (test-draw-point g Color/GREEN -50 0) ; left
   (test-draw-point g Color/RED 50 0) ; right
   (test-draw-point g Color/BLUE 0 50)) ; up
    
;-- Menu items --;

(def mi-hello
   (proxy [ActionListener] []
      (actionPerformed [e] (println "Clicked!" e))))

;-- Components --;

(def ^{:doc "Menu bar for window."}
   menu
   (doto (JMenuBar.)
      (.add (doto (JMenu. "Spline")
               (.add (doto (JMenuItem. "Hello!")
                        (.addActionListener mi-hello)))))))

(def ^{:doc "Control panel" :tag JPanel}
   controls
   (doto (JPanel.)
      (.setMinimumSize (Dimension. 300 600))))


(declare ^JComponent canvas)

(defn update-canvas-depends!
   "Update variables that depend on the canvas size or other state."
   []
   (dosync
      (let [^Dimension dim (.getSize canvas)
            [dim-w dim-h] (de-dim dim)]
         (ref-set view-center (Point2D$Double. (/ dim-w 2) (/ dim-h 2)))
         (ref-set viewport-dim dim)
         (update-xform!))))

(def ^{:doc "Drawing canvas" :tag JComponent}
   canvas
   (doto (proxy [JComponent] []
            (paint [^Graphics2D g] (render g)))
      (.setDoubleBuffered true)
      (.setMinimumSize (Dimension. 10 10))
      (.setPreferredSize (Dimension. 600 600))
      (.addComponentListener
         (proxy [ComponentAdapter] []
            (componentResized [_] (update-canvas-depends!))))))

;-- Setup --;

(defn launch
   "Create and display the GUI."
   []
   (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
   (let [frame (doto (JFrame.)
                  (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                  (.setJMenuBar menu)
                  (.setLayout (BorderLayout. 0 0))
                  (.add controls BorderLayout/LINE_START)
                  (.add canvas BorderLayout/CENTER)
                  (.pack))]
      (update-canvas-depends!)
      (.setVisible frame true)))

(defn -main
   "Main sequence" ;FIXME
   [& args]
   (SwingUtilities/invokeLater launch))


