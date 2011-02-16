(ns timmcHW3.core
   "Core code. Use -main."
   (:import [javax.swing SwingUtilities UIManager
                         JFrame JComponent JPanel JMenu JMenuBar JMenuItem JButton
                         KeyStroke]
            [java.awt BorderLayout Graphics2D RenderingHints Dimension Color BasicStroke Component]
            [java.awt.geom AffineTransform Path2D Path2D$Double Point2D Point2D$Double Line2D Line2D$Double Rectangle2D$Double Ellipse2D Ellipse2D$Double]
            [java.awt.event ActionListener ComponentAdapter MouseAdapter MouseEvent MouseMotionAdapter MouseWheelListener])
   (:gen-class))

(defrecord ^{:doc "GUI components."}
   GUI
   [^{:doc "Application window." :tag JFrame}
      frame
      ^{:doc "Toolbox buttons." :tag JPanel}
      controls
      ^{:doc "Drawing canvas." :tag JComponent}
      canvas
      ^{:doc "Application menubar." :tag JMenuBar}
      menu
      ^{:doc "Undo menu item." :tag JMenuItem}
      mi-undo
      ^{:doc "Redo menu item." :tag JMenuItem}
      mi-redo
      ])
    
(def ^{:doc "The viewpoint's state."}
   gui
   (ref (GUI. nil nil nil nil nil nil)))
    
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

(def ^{:doc "The viewpoint's state."}
   view
   (ref (Viewpoint. [0 0]
                    200
                    0
                    [0 0]
                    (AffineTransform.)
                    (AffineTransform.)
                    (Dimension. 1 1))))

(defn ^AffineTransform calc-xform
   "Calculate the world-to-viewport transformation."
   [view-w view-h]
   (dosync
      (let [[drag-x drag-y] (.rot-center @view)
            minspect (min view-w view-h)
            magnification (/ minspect (.view-minspect @view))]
         (doto (AffineTransform.)
            (.translate (/ view-w 2) (/ view-h 2))
            (.rotate (- (.view-rot @view)))
            (.scale 1 -1) ; flip y coords
            (.scale magnification magnification) ; zoom
            (.translate (- drag-x) (- drag-y))))))

(defn update-xform!
   "Update the world-to-viewpoint and inverse transformations."
   []
   (dosync
      (let [[w h] (de-dim (.viewport-dim @view))
            at (calc-xform w h)]
          (assoc-in-ref! view
             [:xform-to-view] at
             [:xform-from-view] (.createInverse at)))))

;-- State --;

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
   ])

(def ^{:doc "Global pointer to current state."} state
   (ref (ProgState. :initialization)))

;-- Data --;

(defrecord ^{:doc "Current state of user's data. This is saved in undo/redo buffers."}
   UserData
   [act ; The act that produced this state, e.g. "vertex drag" or empty string.
    curves ; Vector of cubic Bézier curves, each of which is a list of 4+ Point2Ds.
    pending-points ; Vector of Point2Ds that have not been incorporated into a curve yet.
   ])

(def ^{:doc "User data that needs undo/redo."} udata
   (ref (UserData. "Initialization" [] [])))

(defn ^{:udata-sel [] :actname "add vertex"}
   add-pending-point
   "Add a pending point to the world."
   [^UserData u, ^Point2D wp]
   (let [new-pend (conj (:pending-points u) wp)]
      (assoc u :pending-points new-pend)))

;-- History --;

(def ^{:doc "Undo buffer."}
   data-past (ref ()))

(def ^{:doc "Redo buffer."}
   data-future (ref ()))

(defn can-undo?
   "Return true if there is undo history."
   []
   (not (empty? @data-past)))

(defn can-redo?
   "Return true if there is redo history."
   []
   (not (empty? @data-future )))

(defn reflect-history-state!
   "Reflect current undo/redo state into GUI."
   []
   (let [^JMenuItem mi-undo (.mi-undo @gui)
         ^JMenuItem mi-redo (.mi-redo @gui)]
      (if (can-undo?)
         (doto mi-undo
            (.setEnabled true)
            (.setText (str "Undo " (.act @udata))))
         (doto mi-undo
            (.setEnabled false)
            (.setText "Nothing to undo")))
      (if (can-redo?)
         (doto mi-redo
            (.setEnabled true)
            (.setText (str "Redo " (.act (first @data-future)))))
         (doto mi-redo
            (.setEnabled false)
            (.setText "Nothing to redo")))))
   
; TODO define helper arity to take a keyword for just acting on that element
(defn act!
   "Call f with current *udata* state and any additional arguments, accepting result as new state. The 'overrides' map arg may override :udata-sel and :actname metadata found on f."
   [f overrides & args]
   (dosync
      (let [cur-state @udata
            fkeys (select-keys (meta f) [:actname])
            metadata (merge {:udata-sel [] :actname ""} fkeys overrides);FIXME strings not showing properly on undo ; defaults, function-specified, overrides
            next-state (apply update-in0 cur-state (:udata-sel (meta f)) f args)
            next-state (assoc next-state :act (:actname metadata))]
         (ref-set data-past (conj @data-past cur-state))
         (ref-set data-future ()) ; destroy the future
         (ref-set udata next-state)))
   (reflect-history-state!))

(defn slide-history!
   "Push current state onto 'to' and pop 'from' as new state."
   [from state to]
   (dosync
      (ref-set to (conj @to @state))
      (ref-set udata (first @from))
      (ref-set from (rest @from))))

;-- Math --;

;TODO: add {set,nudge}-{rotation,zoom,translation-{x,y}}! functions
;TODO: add functions to apply transforms to collections of coords

(defn ^Path2D view-cubic-curve
   "Calculate the path based on the current control points."
   [^Point2D p0, ^Point2D p1, ^Point2D p2, ^Point2D p3]
   (let [^Path2D path (Path2D$Double.)]
      (.moveTo path (.getX p0) (.getY p0))
      (.curveTo path
         (.getX p1) (.getY p1)
         (.getX p2) (.getY p2)
         (.getX p3) (.getY p3))
      (.createTransformedShape ^AffineTransform (.xform-to-view @view) path)))

(defn ^Point2D transform
   "Transform a location from one coordinate system to another."
   ([^AffineTransform at, sx, sy]
    (.transform at (Point2D$Double. sx sy) nil))
   ([^AffineTransform at, ^Point2D p]
    (.transform at p nil)))

(defn ^Point2D loc-to-view
   "Transform a location from world to viewport coords."
   [& args]
   (apply transform (.xform-to-view @view) args))

(defn ^Point2D loc-from-view
   "Transform a location from viewport to world coords."
   [& args]
   (apply transform (.xform-from-view @view) args))

;-- Rendering --;

(defn test-draw-point
   "Draw a test point at the given coords."
   [^Graphics2D g, ^Color c, wx, wy]
   (let [[vx vy] (de-pt (loc-to-view wx wy))]
      (doto g
         (.setPaint c)
         (.fill (Rectangle2D$Double. (- vx 3) (- vy 3) 6 6)))))

(def ^Color control-sement-color Color/YELLOW)
(def ^BasicStroke control-segment-stroke
   (BasicStroke. (float 2.5) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))

(defn draw-control-segments
   "Draw segments of a control polygon."
   [^Graphics2D g, points]
   (loop [^Point2D prev (first points)
          remain (next points)]
      (when remain
         (let [cur (first remain)]
            (.setColor g Color/BLUE)
            (.setStroke g control-segment-stroke)
            (.draw g (Line2D$Double. (loc-to-view prev) (loc-to-view cur)))
            (recur cur (next remain))))))

(def ^Color control-point-color Color/GREEN)

(defn draw-control-points
   "Draw vertices of a control polygon."
   [^Graphics2D g, points]
   (doseq [p points]
      (let [[vcx vcy] (de-pt (loc-to-view p))]
         (.setColor g control-point-color)
         (.fill g (Ellipse2D$Double. (- vcx 3) (- vcy 3) 6 6)))))

(def ^Color curve-pending-color Color/RED)
(def ^Color spline-color Color/RED)
(def ^BasicStroke curve-stroke
   (BasicStroke. (float 2.5) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
    
(defn draw-spline
   "Draw the main user spline"
   [^Graphics2D g]
   (comment "TODO"))

(defn draw-pending
   "Draw a potentially incomplete curve."
   [^Graphics2D g, points]
   (draw-control-segments g points)
   (draw-control-points g points)
   (when (= (count points) 4)
      (.setColor g curve-pending-color)
      (.setStroke g curve-stroke)
      (.draw g (apply view-cubic-curve points))))
    
(defn render
   "Draw the world."
   [^Graphics2D g]
   (let [[w h] (de-dim (.viewport-dim @view))
         [cx cy] (de-pt (.view-center @view))]
      (doto g
         (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
         (.setColor (Color. 50 50 50))
         (.fill (Rectangle2D$Double. 0 0 w h))
         (.setColor Color/YELLOW)
         (.draw (Rectangle2D$Double. cx 0 0.1 h))
         (.draw (Rectangle2D$Double. 0 cy w 0.1))))
   (test-draw-point g Color/WHITE 0 0) ; center
   (test-draw-point g Color/GREEN -50 0) ; left
   (test-draw-point g Color/RED 50 0) ; right
   (test-draw-point g Color/BLUE 0 50) ; up
   (draw-spline g)
   (draw-pending g (.pending-points @udata)))

;=====;
; GUI ;
;=====;

;-- Event handlers --;

(defn undo!
   "Undo to previous state, if possible. Does not trigger redraw."
   []
   (dosync
      (when (can-undo?)
         (slide-history! data-past udata data-future)))
   (reflect-history-state!))
    
(defn redo!
   "Redo to subsequent state, if possible. Does not trigger redraw."
   []
   (dosync
      (when (can-redo?)
         (slide-history! data-future udata data-past)))
   (reflect-history-state!))
    
(defn maybe-exit
   "Exit, or possible ask user to save data first."
   []
   (.dispose (.frame @gui)))

(defn update-canvas-depends!
   "Update variables that depend on the canvas size or other state."
   []
   (dosync
      (let [dim (.getSize (.canvas @gui))
            [dim-w dim-h] (de-dim dim)]
         (assoc-in-ref! view
            [:view-center] (Point2D$Double. (/ dim-w 2) (/ dim-h 2))
            [:viewport-dim] dim)
         (update-xform!))))
    
;-- Event interpretation --;

(defn ask-redraw
   "Ask for the canvas to be redrawn."
   []
   (.repaint (.canvas @gui)))

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

; We define these in functions so they are not created at compile-time.

(defn ^JMenuItem new-mi-undo
   []
   (doto (JMenuItem. "Undo")
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (undo!)
               (ask-redraw))))
      (.setEnabled false)
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Z"))))

(defn ^JMenuItem new-mi-redo
   []
   (doto (JMenuItem. "Redo")
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (redo!)
               (ask-redraw))))
      (.setEnabled false)
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Y"))))

(defn ^JMenuItem new-mi-exit
   []
   (doto (JMenuItem. "Exit")
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (maybe-exit))))
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Q"))))

(defn ^JMenuBar new-menu
   "Make a menu bar."
   []
   (doto (JMenuBar.)
      (.add (doto (JMenu. "Spline")
               (.add (create! gui [:mi-undo] new-mi-undo))
               (.add (create! gui [:mi-redo] new-mi-redo))
               (.add (new-mi-exit))))))

(defn ^JPanel new-controls
   "Make a control panel."
   []
   (doto (JPanel.)
      (.setMinimumSize (Dimension. 300 600))))

(defn ^JComponent new-canvas
   "Make a drawing canvas."
   []
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

(defn ^JFrame new-frame
   "Make the application window."
   []
   (doto (JFrame.)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setJMenuBar (create! gui [:menu] new-menu))
      (.setLayout (BorderLayout. 0 0))
      (.add (create! gui [:controls] new-controls) BorderLayout/LINE_START)
      (.add (create! gui [:canvas] new-canvas) BorderLayout/CENTER)
      (.pack)))

;-- Setup --;

(defn launch
   "Create and display the GUI."
   []
   (let [frame (create! gui [:frame] new-frame)]
      (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
      (update-canvas-depends!)
      (.setVisible frame true)))

(defn -main
   "Start application. Takes no arguments."
   [& args]
   (SwingUtilities/invokeLater launch))


