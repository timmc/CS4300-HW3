(ns timmcHW3.core
   "Core code. Use -main."
   (:import [javax.swing SwingUtilities UIManager
                         JFrame JComponent JPanel JMenu JMenuBar JMenuItem JButton]
            [java.awt BorderLayout Graphics2D RenderingHints Dimension Color Component]
            [java.awt.geom Path2D Path2D$Double Point2D Point2D$Double Rectangle2D$Double]
            [java.awt.event ActionListener ComponentAdapter])
   (:use (timmcHW3 coords))
   (:gen-class))

;-- Conventions --;

;;; Coordinates, points, and 2-vectors are represented as [x y] pairs.
;;; All curve data is represented in standard Cartesian world coordinates.

(defn de-dim
   "Read a Dimension object into a 2-vector of width, height."
   [^Dimension d]
   [(. d width) (. d height)])

(defn de-pt
   "Read a Point2D$Double object into a 2-vector of x, y."
   [^Point2D$Double p]
   [(. p x) (. p y)])

;-- Constants --;

(def ^{:doc "Multiplier for incremental zoom in."}
   zoom-factor 1.1)

;-- Viewpoint --;

; Translation: User drags new world point to center of window.
(def ^{:doc "Chosen offset of rotation."}
   view-offset
   (ref (mk-offset 10 0)))

; Scale: Resizing the window stretches the view.
(def ^{:doc "Minimum extent of world to show in both width and height."}
   view-minspect
   (ref 60))

; Rotation: Around the center of the window.
(def ^{:doc "Rotation of viewport."}
   view-rot (ref (/ Math/PI 4)))

; Centering: This is the center of rotation of the viewpoint.
(def ^{:doc "Viewport's pixel center coordinates as [x y]."}
   view-center
   (ref [1 1]))

(def ^{:doc "World-to-viewport transform."}
   tmat-to-view
   (ref (mat3xm)))

(def ^{:doc "The inverse transform, viewport-to-world."}
   tmat-from-view
   (ref (mat3xm)))

(def ^{:doc "Dimensions of viewport."}
   viewport-dim
   (ref (Dimension. 1 1)))

(defn calc-tmat
   "Calculate the world-to-viewport transformation matrix."
   [view-w view-h]
   (dosync
      (let [drag-x (crd-x @view-offset)
            drag-y (crd-y @view-offset)
            drag-trans (translator (- drag-x) (- drag-y))
            minspect (min view-w view-h)
            zoom (scalor (/ minspect @view-minspect))
            flip (scalor 1 -1)
            rot (rotator (- @view-rot))
            centering-trans (translator (/ view-w 2) (/ view-h 2))]
         (mat3xm centering-trans rot flip zoom drag-trans))))

(defn update-tmat!
   "Update the world-to-viewpoint and inverse transformation matrices."
   []
   (dosync
      (let [[w h] (de-dim @viewport-dim)
            tmat (calc-tmat w h)]
          (ref-set tmat-to-view tmat)
          #_(ref-set tmat-from-view (mat3-inverse tmat))))) ;TODO inverse

;-- Data --;

(def ^{:doc "Control polygon as coords list"} ;TODO better than cubic
   control-polygon (ref [(mk-loc -50 0)
                         (mk-loc 0 30)
                         (mk-loc 0 -30)
                         (mk-loc 5 0)]))

;-- Math --;

;TODO: add {set,nudge}-{rotation,zoom,translation-{x,y}}! functions
;TODO: add functions to apply transforms to collections of coords

(defn ^Path2D calc-path
   "Calculate a path based on the current control points."
   [] ;TODO accept polyline as arg?
   (let [points @control-polygon
         path (Path2D$Double.)]
      (when (= (count points) 4)
         (let [[p0 p1 p2 p3] points]
            (. path moveTo (crd-x p0) (crd-y p0))
            (. path curveTo (crd-x p1) (crd-y p1)
                            (crd-x p2) (crd-y p2)
                            (crd-x p3) (crd-y p3))))
      path))

;-- Rendering --;

(defn test-draw-point
   "Draw a test point at the given coords."
   [^Graphics2D g, ^Color c, x, y]
   (let [[p-x p-y] (de-pt (to-view (mk-loc x y) @tmat-to-view))]
      (. g setPaint c)
      (. g fill (Rectangle2D$Double. (- p-x 3) (- p-y 3) 6 6))))

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
   (test-draw-point g Color/GREEN -10 0) ; left
   (test-draw-point g Color/RED 10 0) ; right
   (test-draw-point g Color/BLUE 0 10)) ; up
    
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
      (let [^Dimension dim (. canvas getSize)
            [dim-w dim-h] (de-dim dim)]
         (ref-set view-center (Point2D$Double. (/ dim-w 2) (/ dim-h 2)))
         (ref-set viewport-dim dim)
         (update-tmat!))))

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
      (. frame setVisible true)))

(defn -main
   "Main sequence" ;FIXME
   [& args]
   (SwingUtilities/invokeLater launch))


