(ns timmcHW3.core
   "Core code. Use -main."
   (:use timmcHW3.utils)
   (:use timmcHW3.drawing)
   (:import
      [javax.swing SwingUtilities UIManager BoxLayout BorderFactory
         JFrame JComponent JPanel JMenu JMenuBar JMenuItem JCheckBoxMenuItem JButton JSpinner SpinnerNumberModel JSpinner$NumberEditor
         KeyStroke]
      [javax.swing.border Border EtchedBorder]
      [javax.swing.event ChangeListener]
      [java.awt Dimension Component BorderLayout
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
    ^{:doc "Spinner for angle of rotation, in radians." :tag JSpinner}
     spinner-rot
    ^{:doc "Spinner for degree of zoom, using default zoom 1. Double mag by adding 0.1." :tag JSpinner}
     spinner-zoom
    ])

(def ^{:doc "The viewpoint's state."} gui (ref nil))

(defn init-gui []
   (dosync (ref-set gui (GUI. nil nil nil nil nil nil nil nil nil))))

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

(def default-rot-center [0 0])
(def default-view-minspect 100)
(def default-view-rot 0.0)

; constants and precomputations for scaling
(def double-step 0.1)
(def standard-scale (Math/log default-view-minspect))
(def double-diff (- (Math/log 2) (Math/log 1)))

(defn minspect-to-zoom
   "Linear to 1-based scale."
   [m]
   (- 1 (* (/ (- (Math/log m) standard-scale) double-diff) double-step)))

(defn zoom-to-minspect
   "1-based scale to linear."
   [z]
   (Math/exp (+ (* (/ (- 1 z) double-step) double-diff) standard-scale)))

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

(defn update-pose!
   "Update view based on current state of Pose spinners."
   []
   (dosync
      (let [rot (double (.getValue (.spinner-rot @gui)))
            minspect (zoom-to-minspect (.getValue (.spinner-zoom @gui)))]
         (assoc-in-ref! view [:view-rot] rot)
         (assoc-in-ref! view [:view-minspect] minspect))
      (update-xform!)))

(defn init-view
   "Initialize the global view ref."
   []
   (let [rot ()]
      (dosync
         (ref-set view
            (Viewpoint. default-rot-center
                        default-view-minspect
                        default-view-rot
                        nil nil nil nil)))
      (update-canvas-depends!)))

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
    saved-mode ; ProgState.mode that was active when this state was *first* saved off.
   ])

(def ^{:doc "User data that needs undo/redo."} udata (ref nil))

(defn init-udata []
   (dosync (ref-set udata (UserData. "Initialization" [] [] (.mode @state)))))

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
      (let [old-state (assoc @udata :saved-mode (.mode @state))]
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
   (draw-control-segments g (map loc-to-view points))
   (draw-control-points g (map loc-to-view points))   
   (when (> (count points) 2)
      (.setColor g curve-pending-color)
      (.setStroke g curve-stroke)
      (.draw g (.createTransformedShape (.xform-to-view @view)
                                        (de-casteljau points 20))))) ; todo: calc appropriate number of interpolations
    
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
   (test-draw-point g Color/GREEN -20 0) ; left
   (test-draw-point g Color/RED 20 0) ; right
   (test-draw-point g Color/BLUE 0 20) ; up
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
   "Determine mode from user data state."
   []
   false ;TODO
   )

(defn reflect-mode!
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
            (assoc-in-ref! state [:mode] (.saved-mode @udata))))
      (ask-redraw)
      (reflect-history-state!)
      (reflect-mode!))) ; TODO reset temporary state like ProgState.posing?

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
         (ask-redraw)
         (reflect-mode!))))

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

(defn ^Border make-controls-border
   [title]
   (-> (BorderFactory/createEtchedBorder EtchedBorder/LOWERED)
       (BorderFactory/createTitledBorder title)
       (BorderFactory/createCompoundBorder (BorderFactory/createEmptyBorder 4 4 4 4))))

(defn ^Component make-vertical-layout
   "Lay out a series of components as a vertical stack with an expanding south. Not tail-recursive."
   [c & more]
   (let [cont (JPanel.)]
      (.setLayout cont (BorderLayout.))
      (.add cont c BorderLayout/NORTH)
      (when (seq more)
         (.add cont (apply make-vertical-layout more) BorderLayout/CENTER))
      cont))

(defn ^JSpinner new-pose-rotate
   "Make the spinner for viewpoint rotation."
   []
   (let [nm (SpinnerNumberModel. default-view-rot nil nil 0.03) ; radians!
         js (JSpinner. nm)
         ned (JSpinner$NumberEditor. js "#####0.000")]
      (-> ned (.getTextField) (.setColumns 5))
      (doto js
         (.setEditor ned)
         (.addChangeListener
            (proxy [ChangeListener] []
               (stateChanged [_]
                  (update-pose!)
                  (ask-redraw)))))))

(defn ^JSpinner new-pose-zoom
   "Make the spinner for viewpoint zooming."
   []
   (let [nm (SpinnerNumberModel. (minspect-to-zoom default-view-minspect) 0.001 20.0 0.003) ; log scale, higher is greater mag
         js (JSpinner. nm)
         ned (JSpinner$NumberEditor. js "#####0.000")]
      (-> ned (.getTextField) (.setColumns 5))
      (doto js
         (.setEditor ned)
         (.addChangeListener
            (proxy [ChangeListener] []
               (stateChanged [_]
                  (update-pose!)
                  (ask-redraw)))))))

(defn ^JPanel new-pose-panel
   "Make a Pose panel"
   []
   (let [p (JPanel.)]
      (doto p
         (.setBorder (make-controls-border "Position"))
         (.add (create! gui [:spinner-rot] new-pose-rotate))
         (.add (create! gui [:spinner-zoom] new-pose-zoom)))))

(defn ^JPanel new-controls
   "Make a control panel."
   []
   (let [p (JPanel.)]
      (doto p
         (.add (make-vertical-layout
                  (new-pose-panel))))))

(defn ^JComponent new-canvas
   "Make a drawing canvas."
   []
   (let [jc (proxy [JComponent] []
               (paint [^Graphics2D g] (render g)))]
      (doto jc
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
;                  (when (.canvas @gui) ; may not be initialized yet
                     (update-canvas-depends!)
                     (ask-redraw)))))));)

(defn ^JFrame new-frame
   "Make the application window."
   []
   (let [fr (JFrame. "CS4300 HW3 - TimMc")]
      (doto fr
         (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
         (.setJMenuBar (create! gui [:menu] new-menu))
         (.setLayout (BorderLayout.))
         (.add (create! gui [:controls] new-controls) BorderLayout/LINE_START)
         (.add (create! gui [:canvas] new-canvas) BorderLayout/CENTER)
         (.pack))))

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

;TODO: Add action listeners after interface is entirely set up?
