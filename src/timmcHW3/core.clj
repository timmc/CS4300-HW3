(ns timmcHW3.core
   "Core code. Use -main."
   (:use
      [timmcHW3.utils]
      [timmcHW3.drawing]
      [timmcHW3.gui])
   (:require timmcHW3.state)
   (:import [timmcHW3.state GUI Viewpoint ProgState UserData])
   (:import
      [javax.swing SwingUtilities UIManager
         JFrame JComponent JPanel JMenuItem JCheckBoxMenuItem JButton JSpinner]
      [javax.swing.event ChangeListener]
      [java.awt Dimension Component
         Graphics2D RenderingHints Color BasicStroke]
      [java.awt.geom AffineTransform Path2D Path2D$Double Point2D Point2D$Double
         Line2D Line2D$Double Rectangle2D$Double Ellipse2D Ellipse2D$Double]
      [java.awt.event ActionListener ComponentAdapter MouseAdapter MouseEvent MouseMotionAdapter MouseWheelListener])
    (:gen-class))

(def ^{:doc "The viewpoint's state."} gui (ref nil))

(defn init-gui []
   (dosync (ref-set gui (GUI. nil nil nil nil nil nil nil nil nil nil))))

;-- Viewpoint --;

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

(def ^{:doc "Global pointer to current state."} state (ref nil))

(defn init-state []
   (dosync (ref-set state (ProgState. :extend0 false false))))

;-- Data --;

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
   (when (.getState (.mi-view-control @gui))
      (draw-control-segments g (map loc-to-view points))
      (draw-control-points g (map loc-to-view points)))
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
         (.fill (Rectangle2D$Double. 0 0 w h))))
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
      (when (and (= (.getButton e) MouseEvent/BUTTON1)
                 (not (.isShiftDown e))
                 (not (.isControlDown e))
                 (not (.posing? @state))
                 (not (.dragging? @state))
                 (.getState (.mi-view-control @gui))
                 (not= (.mode @state) :manipulate))
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

(defn reflect-view!
   "Reflect view state into GUI."
   [rview rgui]
   (doto (.spinner-zoom @rgui)
      (.setValue (minspect-to-zoom (.view-minspect @rview))))
   (doto (.spinner-rot @rgui)
      (.setValue (.view-rot @rview))))

(defn enliven!
   "Add action listeners to GUI components."
   [rgui]
   (doto (.mi-undo @rgui)
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (do-history! true)))))
   (doto (.mi-redo @rgui)
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (do-history! false)))))
   (doto (.mi-view-control @rgui)
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (ask-redraw)))))
   (doto (.mi-exit @rgui)
      (.addActionListener
         (proxy [ActionListener] []
            (actionPerformed [_]
               (do-maybe-exit)))))
   (doto (.spinner-rot @rgui)
      (.addChangeListener
         (proxy [ChangeListener] []
            (stateChanged [_]
               (update-pose!)
               (ask-redraw)))))
   (doto (.spinner-zoom @rgui)
      (.addChangeListener
         (proxy [ChangeListener] []
            (stateChanged [_]
               (update-pose!)
               (ask-redraw)))))
   (doto (.canvas @rgui)
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

;-- Setup --;

(defn launch
   "Create and display the GUI."
   []
   (init-state)
   (init-gui)
   (let [frame (create! gui [:frame] new-frame gui render)]
      (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
      (init-view)
      (init-udata)
      (reflect-view! view gui)
      (update-canvas-depends!)
      (enliven! gui)
      (.setVisible frame true)))

(defn -main
   "Start application. Takes no arguments."
   [& args]
   (SwingUtilities/invokeLater launch))

;TODO: Add action listeners after interface is entirely set up?
