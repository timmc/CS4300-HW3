(ns timmcHW3.core
   "Core code. Use -main."
   (:import [javax.swing SwingUtilities UIManager
                         JFrame JComponent JPanel JMenu JMenuBar JMenuItem JButton
                         KeyStroke]
            [java.awt BorderLayout Graphics2D RenderingHints Dimension Color Component]
            [java.awt.geom AffineTransform Path2D Path2D$Double Point2D Point2D$Double Rectangle2D$Double]
            [java.awt.event ActionListener ComponentAdapter MouseAdapter MouseEvent MouseMotionAdapter MouseWheelListener])
   (:gen-class))
    
(declare *state*)

(defn st
   "Get a piece of state."
   [&ks]
   (get-in @*state* ks))

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

(defmacro assoc-in-ref!
   "Update an associative ref with pairs of key seqs and new values."
   [ref-expr & kv-exprs]
   (when-not (even? (count kv-exprs))
      (throw (IllegalArgumentException. "assoc-in-ref! requires an even number of key-seq/value pairs.")))
   (when (empty? kv-exprs)
      (throw (IllegalArgumentException. "assoc-in-ref! requires at least one key-seq/value pair.")))
   (let [pairs (partition 2 kv-exprs)
         assocs (map #(list 'assoc-in0 (first %) (second %)) pairs)]
      `(let [ref-val# ~ref-expr]
          (dosync
             (ref-set ref-val# (-> (deref ref-val#) ~@assocs))))))


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

(defrecord ^{:doc "Viewport state."}
   Viewpoint
   [^{:doc "Chosen center of rotation."}
     rot-center ; Translation: User drags new world point to center of window.
    ^{:doc "Minimum extent of world to show in both width and height."}
     view-minspect ; Scale: Resizing the window stretches the view.
    ^{:doc "Rotation of viewport."}
     view-rot ; Rotation: Around the center of the window.
    ^{:doc "Viewport's pixel center coordinates as [x y]."}
     view-center ; Centering: This is the center of rotation of the viewpoint.
    ^{:doc "World-to-viewport transform." :tag AffineTransform}
     xform-to-view
    ^{:doc "The inverse transform, viewport-to-world." :tag AffineTransform}
     xform-from-view
    ^{:doc "Dimensions of viewport." :tag Dimension}
     viewport-dim
   ])

(defn ^Viewpoint new-viewport
   "Make an initial viewpoint."
   []
   (Viewpoint. [0 0]
               200
               0
               [0 0]
               (AffineTransform.)
               (AffineTransform.)
               (Dimension. 1 1)))

(defn ^AffineTransform calc-xform
   "Calculate the world-to-viewport transformation."
   [view-w view-h]
   (dosync
      (let [[drag-x drag-y] (st :view :rot-center)
            minspect (min view-w view-h)
            magnification (/ minspect (st :view :view-minspect))]
         (doto (AffineTransform.)
            (.translate (/ view-w 2) (/ view-h 2))
            (.rotate (- (st :view :view-rot)))
            (.scale 1 -1) ; flip y coords
            (.scale magnification magnification) ; zoom
            (.translate (- drag-x) (- drag-y))))))

(defn update-xform!
   "Update the world-to-viewpoint and inverse transformations."
   []
   (dosync
      (let [[w h] (de-dim (st :view :viewport-dim))
            at (calc-xform w h)]
          (assoc-in-ref! *state*
             [:view :xform-to-view] at
             [:view :xform-from-view] (.createInverse at)))))

;-- State --;

(defrecord ^{:doc "Current state of user's data. This is saved in undo/redo buffers."}
   UserData
   [act ; The act that produced this state, e.g. "vertex drag"
    curves ; List of cubic Bézier curves, each of which is a list of 4 Point2Ds.
    pending-points ; Point2Ds (in latest-first order) that have not been incorporated into a curve yet.
   ])

(defn ^UserData new-userdata
   "Make default userdata."
   []
   (UserData. "Initialization" [] []))
    
;;; Modes:
; :initializing
; :extend0 - Wait for sufficient input to define new curve. Allow vertex input.
; :extend1 - Wait for indication that new curve is done. Allow vertex input or manipulation.
; :manipulate - Allow dragging of vertices.
; :pose - Viewport may be respositioned.

;;; Other properties:
; :is-dragging
; :selection

(defrecord ^{:doc "Whole-program state."}
   ProgState
   [mode ; overall mode
    view ; viewpoint
    udata ; user data
    data-past ; Undo buffer
    data-future ; Redo buffer
   ])

(defn ^ProgState new-progstate
   "Create default program state."
   []
   (ProgState. :initialization (new-viewport) (new-userdata) () ()))

(def ^{:doc "Global pointer to current state."} *state*
   (ref (make-progstate)))

;-- Data --;

(defn ^{:statekeys [] :actname "add vertex"}
   add-pending-point
   "Add a pending point to the world."
   [state wp]
   (let [old-pending (:pending-points state)
         new-pend (conj (if (= (count old-pending) 4)
                          () ; clear it out if too many
                          old-pending)
                     wp)]
      (assoc state :pending-points new-pend)))

;-- History --;

(defn can-undo?
   "Return true if there is undo history."
   []
   (not (empty? (:data-past @*state*))))

(defn can-redo?
   "Return true if there is redo history."
   []
   (not (empty? (:data-future @*state*))))

(defn reflect-history-state!
   "Reflect current undo/redo state into GUI."
   []
   (if (can-undo?)
      (doto mi-undo
         (.setEnabled true)
         (.setText (str "Undo " (:act @user-data))))
      (doto mi-undo
         (.setEnabled false)
         (.setText "Nothing to undo")))
   (if (can-redo?)
      (doto mi-redo
         (.setEnabled true)
         (.setText (str "Redo " (:act (first @data-future)))))
      (doto mi-redo
         (.setEnabled false)
         (.setText "Nothing to redo"))))
    
; TODO define helper arity to take a keyword for just acting on that element
(defn act!
   "Call f with current user-data state and any additional arguments, accepting result as new state. The 'overrides' map arg may override :statekey and :actname metadata found on f."
   [f overrides & args]
   (dosync
      (let [cur-state (:udata @*state*)
            fkeys (select-keys (meta f) [:actname])
            metadata (merge fkeys overrides)
            next-state (apply up-in0 cur-state (:statekeys (meta f)) f args)
            next-state (assoc next-state :act (:actname metadata))]
         (ref-set data-past (conj (:data-past @*state*) cur-state))
         (ref-set data-future ()) ; destroy the future
         (ref-set user-data next-state)))
   (reflect-history-state!))

(defn slide-history!
   "Push current state onto 'to' and pop 'from' as new state."
   [from state to]
   (dosync
      (ref-set to (conj @to @state))
      (ref-set user-data (first @from))
      (ref-set from (rest @from))))

(defn undo!
   "Undo to previous state, if possible. Does not trigger redraw."
   []
   (dosync
      (when (can-undo?)
         (slide-history! data-past user-data data-future)))
   (reflect-history-state!))

(defn redo!
   "Redo to subsequent state, if possible. Does not trigger redraw."
   []
   (dosync
      (when (can-redo?)
         (slide-history! data-future user-data data-past)))
   (reflect-history-state!))

;-- Math --;

;TODO: add {set,nudge}-{rotation,zoom,translation-{x,y}}! functions
;TODO: add functions to apply transforms to collections of coords

(defn ^Path2D calc-path
   "Calculate a path based on the current control points."
   [] ;TODO accept polyline as arg?
   (let [points (reverse (:pending-points @user-data))
         ^Path2D path (Path2D$Double.)]
      (when (= (count points) 4)
         (let [[^Point2D p0
                ^Point2D p1
                ^Point2D p2
                ^Point2D p3] points]
            (.moveTo path (.getX p0) (.getY p0))
            (.curveTo path (.getX p1) (.getY p1)
                           (.getX p2) (.getY p2)
                           (.getX p3) (.getY p3))))
      (.createTransformedShape ^AffineTransform (st :view :xform-to-view) path)))

(defn ^Point2D transform
   "Transform a location from one coordinate system to another."
   ([^AffineTransform at, sx, sy]
    (.transform at (Point2D$Double. sx sy) nil))
   ([^AffineTransform at, ^Point2D p]
    (.transform at p nil)))

(defn ^Point2D loc-to-view
   "Transform a location from world to viewport coords."
   [& args]
   (apply transform (st :view :xform-to-view) args))

(defn ^Point2D loc-from-view
   "Transform a location from viewport to world coords."
   [& args]
   (apply transform (st :view :xform-from-view) args))

;-- Rendering --;

(defn test-draw-point
   "Draw a test point at the given coords."
   [^Graphics2D g, ^Color c, wx, wy]
   (let [[vx vy] (de-pt (loc-to-view wx wy))]
      (doto g
         (.setPaint c)
         (.fill (Rectangle2D$Double. (- vx 3) (- vy 3) 6 6)))))

(defn draw-spline
   "Draw the main user spline")

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
   (test-draw-point g Color/BLUE 0 50) ; up
   (draw-spline g)
   (draw-pending g))

;-- Event interpretation --;

(defn ask-redraw
   "Ask for the canvas to be redrawn."
   []
   (.repaint canvas))

(defn canvas-click
   "A click event has occurred on the canvas."
   [^MouseEvent e]   
   (act! add-pending-point {} (loc-from-view (.getX e) (.getY e))) ;TODO restrict to acting only during drawing mode
   (ask-redraw)
   )

(defn canvas-drag
   [^MouseMoveEvent e]
   );TODO

(defn canvas-scroll
   [^MouseWheelEvent e]
   );TODO

;-- Components --;

;TODO: Do event handlers need to use (binding *state*)?

(defn ^JMenuItem new-mi-undo
   (doto (JMenuItem. "Undo")
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (undo!)
               (ask-redraw))))
      (.setEnabled false)
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Z"))))

(defn ^JMenuItem new-mi-redo
   (doto (JMenuItem. "Redo")
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (redo!)
               (ask-redraw))))
      (.setEnabled false)
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Y"))))

(defn ^{:doc "Menu bar for window."} new-menubar
   
   (doto (JMenuBar.)
      (.add (doto (JMenu. "Spline")
               (.add (new-mi-undo))
               (.add (new-mi-redo))))))

(def ^{:doc "Control panel" :tag JPanel}
   controls
   (let [])
   (doto (JPanel.)
      (.setMinimumSize (Dimension. 300 600))))


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
      (.addMouseListener
         (proxy [MouseAdapter] []
            (mouseClicked [e] (canvas-click e))))
      (.addMouseMotionListener
         (proxy [MouseMotionAdapter] []
            (mouseDragged [e] (canvas-drag e))))
      (.addMouseWheelListener
         (proxy [MouseWheelListener] []
            (mouseWheelMoved [e] (canvas-scroll e))))
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


