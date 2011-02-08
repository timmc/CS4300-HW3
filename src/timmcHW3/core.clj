(ns timmcHW3.core
   "Core code. Use -main."
   (:import [javax.swing SwingUtilities UIManager
                         JFrame JComponent JPanel JMenu JMenuBar JMenuItem JButton]
            [java.awt Graphics2D RenderingHints Dimension Color]
            [java.awt.geom Path2D Path2D$Double Point2D Point2D$Double]
            [java.awt.event ActionListener])
   (:gen-class))

;-- State --;

(def ^{:doc "Control polygon as Point2D list"} ;TODO better than cubic
   control-polygon (ref [(Point2D$Double. 100 100)
                         (Point2D$Double. 100 150)
                         (Point2D$Double. 150 150)
                         (Point2D$Double. 150 100)]))

;-- Math --;

(defn ^Path2D calc-path
   "Calculate a path based on the current control points."
   [] ;TODO accept polyline as arg?
   (let [points @control-polygon
         path (Path2D$Double.)]
      (when (= (count points) 4)
         (let [[^Point2D$Double p0
                ^Point2D$Double p1
                ^Point2D$Double p2
                ^Point2D$Double p3] points]
            (. path moveTo (. p0 x) (. p0 y))
            (. path curveTo (. p1 x) (. p1 y)
                            (. p2 x) (. p2 y)
                            (. p3 x) (. p3 y))))
      path))

;-- Rendering --;

(defn render
   "Draw the world."
   [^Graphics2D g]
   (. g setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
   (. g setColor Color/RED)
   (. g draw (calc-path)))

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
      (.setPreferredSize (Dimension. 600 600))))

;-- Setup --;

(defn launch
   "Create and display the GUI."
   []
   (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
   (let [frame (doto (JFrame.)
                  (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                  (.setJMenuBar menu)
                  (.add controls)
                  (.add canvas)
                  (.pack))]
      (. frame setVisible true)))

(defn -main
   "Main sequence" ;FIXME
   [& args]
   (SwingUtilities/invokeLater launch))


