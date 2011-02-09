(ns timmcHW3.core
   "Core code. Use -main."
   (:import [javax.swing SwingUtilities UIManager
                         JFrame JComponent JPanel JMenu JMenuBar JMenuItem JButton]
            [java.awt FlowLayout Graphics2D RenderingHints Dimension Color]
            [java.awt.geom Path2D Path2D$Double Point2D Point2D$Double Rectangle2D$Double]
            [java.awt.event ActionListener])
   (:use (timmcHW3 coords))
   (:gen-class))

;-- State --;

(def ^{:doc "Control polygon as coords list"} ;TODO better than cubic
   control-polygon (ref [(mk-loc -50 0)
                         (mk-loc 0 30)
                         (mk-loc 0 -30)
                         (mk-loc 5 0)]))

(defn ^Point2D$Double basic-trans ;XXX
   "Basic transformation"
   [wx wy]
   (let [rot [[0.707 -0.707 0][0.707 0.707 0][0 0 1]]
         scale [[0.5 0 0][0 -0.5 0][0 0 1]]
         trans [[0 0 300][0 0 300][0 0 1]]
         tmat (mat3xm trans rot scale)
         [vx vy _] (mat3xv tmat [wx wy 1])]
      (println wx wy "->" vx vy)
      (Point2D$Double. vx vy)))

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

(defn render
   "Draw the world."
   [^Graphics2D g]
   (let [center (basic-trans 0 0)]
      (. g setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (. g setColor Color/RED)
      (. g fill (Rectangle2D$Double. (. center x) (. center y) 3 3))
      (. g draw (calc-path))))

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

(def ^{:doc "Drawing canvas" :tag JComponent}
   canvas
   (doto (proxy [JComponent] []
            (paint [^Graphics2D g] (render g)))
      (.setDoubleBuffered true)
      (.setPreferredSize (Dimension. 600 600))))

;-- Setup --;

(defn launch
   "Create and display the GUI."
   []
   (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
   (let [frame (doto (JFrame.)
                  (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                  (.setJMenuBar menu)
                  (.setLayout (FlowLayout. FlowLayout/LEFT 0 0))
                  (.add controls)
                  (.add canvas)
                  (.pack))]
      (. frame setVisible true)))

(defn -main
   "Main sequence" ;FIXME
   [& args]
   (SwingUtilities/invokeLater launch))


