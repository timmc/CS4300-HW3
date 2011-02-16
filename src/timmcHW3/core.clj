(ns timmcHW3.core
   "Core code. Use -main."
   (:import
      [javax.swing SwingUtilities UIManager
         JFrame JComponent JPanel JMenu JMenuBar JMenuItem JCheckBoxMenuItem JButton
         KeyStroke]
      [java.awt BorderLayout Dimension Component
         Graphics2D RenderingHints Color BasicStroke]
      [java.awt.geom AffineTransform Path2D Path2D$Double Point2D Point2D$Double
         Line2D Line2D$Double Rectangle2D$Double Ellipse2D Ellipse2D$Double]
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
    ^{:doc "Pose mode toggle button." :tag JCheckBoxMenuItem}
     mi-pose
    ])

(def ^{:doc "The viewpoint's state."} gui (ref nil))

(defn init-gui []
   (dosync (ref-set gui (GUI. nil nil nil nil nil nil nil))))
    
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

(defn assoc-in-ref!
   "Update an associative ref with a new value given a key vector. Evals to true if changed, false otherwise."
   [aref ks v]
   (let [old-val (get-in0 @aref ks)]
      (if (= v old-val)
         false
         (dosync
            (ref-set aref (assoc-in0 @aref ks v))
            true))))


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
    ^{:doc "Viewport's pixel center coordinates as Point2D."}
     view-center ; Centering: This is the center of rotation of the viewpoint.
    ^{:doc "World-to-viewport transform." :tag AffineTransform}
     xform-to-view
    ^{:doc "The inverse transform, viewport-to-world." :tag AffineTransform}
     xform-from-view
    ^{:doc "Dimensions of viewport." :tag Dimension}
     viewport-dim
   ])

(def ^{:doc "The viewpoint's state."} view (ref nil))

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
          (assoc-in-ref! view [:xform-to-view] at)
          (assoc-in-ref! view [:xform-from-view] (.createInverse at)))))

(defn update-canvas-depends!
   "Update variables that depend on the canvas size or other state."
   []
   (dosync
      (let [dim (.getSize (.canvas @gui))
              [dim-w dim-h] (de-dim dim)]
         (assoc-in-ref! view [:view-center] (Point2D$Double. (/ dim-w 2) (/ dim-h 2)))
         (assoc-in-ref! view [:viewport-dim] dim)))
   (update-xform!))
    
(defn init-view []
   (dosync (ref-set view (Viewpoint. [0 0] 200 0 nil nil nil nil)))
   (update-canvas-depends!))

;-- State --;

;;; Modes:
; :extend0 - Wait for sufficient input to define new curve. Allow vertex input.
; :extend1 - Wait for indication that new curve is done. Allow vertex input or manipulation.
; :manipulate - Allow dragging of vertices.

(defrecord ^{:doc "Whole-program state."}
   ProgState
   [mode ; overall mode, explained above
    posing? ; true if in the Pose minor mode
    dragging? ; true if something is being dragged
   ])

(def ^{:doc "Global pointer to current state."} state (ref nil))

(defn init-state []
   (dosync (ref-set state (ProgState. :extend0 false false))))

;-- Data --;

(defrecord ^{:doc "Current state of user's data. This is saved in undo/redo buffers."}
   UserData
   [act ; The act that produced this state, e.g. "vertex drag" or empty string.
    curves ; Vector of cubic Bézier curves, each of which is a list of 4+ Point2Ds.
    pending-points ; Vector of Point2Ds that have not been incorporated into a curve yet.
    saved-view ; Viewpoint that was active when this state was *first* saved off.
    saved-mode ; ProgState.mode that was active when this state was *first* saved off.
   ])

(def ^{:doc "User data that needs undo/redo."} udata (ref nil))

(defn init-udata []
   (dosync (ref-set udata (UserData. "Initialization" [] [] @view (.mode @state)))))

(def add-pending-point ^{:actname "add vertex" :doc "Add a pending point to the world."}
   (fn [^Point2D wp]
       (let [new-pend (conj (:pending-points @udata) wp)]
          (assoc-in-ref! udata [:pending-points] new-pend))))

;-- History --;

(def ^{:doc "Undo buffer."}
   data-past (ref ()))

(def ^{:doc "Redo buffer."}
   data-future (ref ()))

(defn has-history?
   "Return true if there is history in this ref."
   [r]
   (seq @r))

(defn reflect-history-state!
   "Reflect current undo/redo state into GUI."
   []
   (let [^JMenuItem mi-undo (.mi-undo @gui)
         ^JMenuItem mi-redo (.mi-redo @gui)]
      (if (has-history? data-past)
         (doto mi-undo
            (.setEnabled true)
            (.setText (str "Undo " (.act @udata))))
         (doto mi-undo
            (.setEnabled false)
            (.setText "Nothing to undo")))
      (if (has-history? data-future)
         (doto mi-redo
            (.setEnabled true)
            (.setText (str "Redo " (.act (first @data-future)))))
         (doto mi-redo
            (.setEnabled false)
            (.setText "Nothing to redo")))))

(defn act!
   "Call ref-updating f (no other side effects) with arguments. Uses :actname metadata found on f to add to undo buffer."
   [f & args]
   (dosync
      (let [old-state (assoc (assoc @udata :saved-view @view) :saved-mode (.mode @state))]
         (apply f args) ; change @udata
         (let [new-state (assoc @udata :act (:actname (meta f)))]
            (ref-set data-past (conj @data-past old-state))
            (ref-set data-future ()) ; destroy the future
            (ref-set udata new-state)))
   (reflect-history-state!)))

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

(defn ask-redraw
   "Ask for the canvas to be redrawn."
   []
   (.repaint (.canvas @gui)))

(defn update-mode!
   "Update mode-dependent GUI elements."
   []
   false ;TODO
   )

;-- Event handlers --;

(defn slide-history!
   "Slide history for undo/redo."
   [from to]
   (dosync
      (ref-set to (conj @to @udata))
      (ref-set udata (first @from))
      (ref-set from (rest @from))))
   
(defn do-history!
   [undo?]
   (let [from (if undo? data-past data-future)
         to (if undo? data-future data-past)]
      (dosync
         (when (has-history? from)
            (slide-history! from to)
            (assoc-in-ref! view [] (.saved-view @udata))
            (assoc-in-ref! state [:mode] (.saved-mode @udata))))
      (update-canvas-depends!) ; canvas may have changed shape
      (ask-redraw)
      (reflect-history-state!)
      (update-mode!)))

(defn do-maybe-exit
   "Exit, or possible ask user to save data first."
   []
   (.dispose (.frame @gui)))

(defn do-mode-position
   "Update state.posing? if allowed."
   []
   (assoc-in-ref! state [:posing?] (.getState (.mi-pose @gui))))

;-- Event interpretation --;

(defn canvas-click
   "A click event has occurred on the canvas."
   [^MouseEvent e]
   (dosync
      (when-not (or (.posing? @state)
                    (.dragging? @state)
                    (= (.mode @state) :manipulate))
         (act! add-pending-point (loc-from-view (.getX e) (.getY e)))
         (update-mode!)
         (ask-redraw))))

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
               (do-history! true))))
      (.setEnabled false)
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Z"))))

(defn ^JMenuItem new-mi-redo
   []
   (doto (JMenuItem. "Redo")
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (do-history! false))))
      (.setEnabled false)
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Y"))))

(defn ^JMenuItem new-mi-exit
   []
   (doto (JMenuItem. "Exit")
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (do-maybe-exit))))
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Q"))))

(defn ^JCheckBoxMenuItem new-mi-pose
   []
   (doto (JCheckBoxMenuItem. "Position" (.posing? @state))
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (do-mode-position))))))

(defn ^JMenuBar new-menu
   "Make a menu bar."
   []
   (doto (JMenuBar.)
      (.add (doto (JMenu. "Spline")
               (.add (create! gui [:mi-undo] new-mi-undo))
               (.add (create! gui [:mi-redo] new-mi-redo))
               (.add (new-mi-exit))))
      (.add (doto (JMenu. "Mode")
               (.add (create! gui [:mi-pose] new-mi-pose))))))

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
            (componentResized [_]
               (update-canvas-depends!)
               (ask-redraw))))))

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
   (init-state)
   (init-gui)
   (let [frame (create! gui [:frame] new-frame)]
      (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
      (init-view)
      (init-udata)
      (update-canvas-depends!)
      (.setVisible frame true)))

(defn -main
   "Start application. Takes no arguments."
   [& args]
   (SwingUtilities/invokeLater launch))


