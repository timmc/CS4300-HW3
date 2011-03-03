(ns timmcHW3.core
  "Core code. Use -main."
  (:use
   [timmcHW3.utils]
   [timmcHW3.drawing]
   [timmcHW3.gui])
  (:use timmcHW3.state)
  (:import [timmcHW3.state GUI Viewpoint ProgState UserData])
  (:require [timmcHW3.cascade :as dirt])
  (:require [timmcHW3.history :as hist])
  (:import
   [javax.swing SwingUtilities UIManager
    JFrame JComponent JPanel JMenuItem JCheckBoxMenuItem JButton JSpinner]
   [javax.swing.event ChangeListener]
   [java.awt Dimension Component
    Graphics2D RenderingHints Color BasicStroke]
   [java.awt.geom AffineTransform Path2D Path2D$Double Point2D Point2D$Double
    Line2D Line2D$Double Rectangle2D Rectangle2D$Double
    Ellipse2D Ellipse2D$Double]
   [java.awt.event ActionListener ComponentAdapter
    MouseAdapter MouseEvent MouseMotionAdapter MouseWheelListener])
  (:gen-class))

(def ^{:doc "The viewpoint's state."} *gui* (ref nil))
(def ^{:doc "The viewpoint's state."} *view* (ref nil))
(def ^{:doc "Global pointer to current state."} *state* (ref nil))
(def ^{:doc "User data that needs undo/redo."} *udata* (ref nil))

(def ^{:doc "State dirtiness cascade."} *cascade* (ref nil))

(defn dirty!
  "Mark one or more pieces of state as dirty."
  [& kws]
  (dosync
   (ref-set *cascade* (reduce dirt/dirty @*cascade* kws))
   nil))

(defn clean!
  "Clean up state and re-display where necessary."
  ([& kws]
     (apply dirty! kws)
     (clean!))
  ([]
     (dosync
      (ref-set *cascade* (dirt/clean @*cascade* :all))
      nil)))

(def ^{:doc "Undo/redo buffers."} *history* (ref nil))

;;;-- Mode accessors --;;;

(defn curve-has-extent?
  "Return true if we can define the size of the control polygon."
  []
  (>= (count (.curve ^UserData @*udata*)) 2))

(defn at-least-cubic?
  "Return true if we are showing enough points to draw a non-trivial curve."
  []
  (>= (count (.curve ^UserData @*udata*)) 3))

(defn show-control-poly?
  "Check whether we are showing the control polygon."
  []
  (or (.getState (.mi-view-control @*gui*))
      (not (at-least-cubic?))))

;;;-- Viewpoint --;;;

(def default-rot-center (Point2D$Double. 0 0))
(def default-view-minspect 100)
(def default-view-rot 0.0)

;;; constants and precomputations for scaling
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

(defn ^AffineTransform calc-xform
  "Calculate the world-to-viewport transformation."
  [view-w view-h]
  (dosync
   (let [[drag-x drag-y] (de-pt (.rot-center @*view*))
         minspect (min view-w view-h)         
         magnification (/ minspect (.view-minspect @*view*))]
     (doto (AffineTransform.)
       (.translate (/ view-w 2) (/ view-h 2))
       (.rotate (- (.view-rot @*view*)))
       (.scale 1 -1) ; flip y coords
       (.scale magnification magnification) ; zoom
       (.translate (- drag-x) (- drag-y))))))

(defn update-xform!
  "Update the world-to-viewpoint and inverse transformations."
  []
  (dosync
   (let [[w h] (de-dim (.viewport-dim @*view*))
         at (calc-xform w h)]
     (assoc-in-ref! *view* [:xform-to-view] at)
     (assoc-in-ref! *view* [:xform-from-view] (.createInverse at)))))

(defn update-canvas-shape!
  "Update variables that depend on the canvas size or other state."
  []
  (dosync
   (let [dim (.getSize (.canvas @*gui*))
         [dim-w dim-h] (de-dim dim)]
     (assoc-in-ref! *view* [:view-center]
                    (Point2D$Double. (/ dim-w 2) (/ dim-h 2)))
     (assoc-in-ref! *view* [:viewport-dim] dim))))

(defn update-pose!
  "Update view based on current state of Pose spinners."
  []
  (dosync
   (let [rot (double (.getValue (.spinner-rot @*gui*)))
         minspect (zoom-to-minspect (.getValue (.spinner-zoom @*gui*)))]
     (assoc-in-ref! *view* [:view-rot] rot)
     (assoc-in-ref! *view* [:view-minspect] minspect))))

;;;-- User data modifiers --;;;

(defn append-vertex!
  "Add a vertex to the curve."
  [^Point2D wp]
  (dosync
   (assoc-in-ref! *udata* [:curve]
                  (conj (.curve @*udata*) wp))
   (dirty! :udata)))

(defn replace-vertex!
  "Replace an old world-vertex with a new one, mark dirty."
  [^Point2D old,^Point2D new]
  (when (nil? new)
    (throw (IllegalArgumentException. "Given nil replacement vertex.")))
  (dosync
   ;;; (into [] ...) keeps it a vector, and in the right order 
   (assoc-in-ref! *udata* [:curve]
                  (into [] (map #(if (identical? % old) new %)
                                (.curve @*udata*))))
   (dirty! :udata)))

;;;-- History --;;;

(defn update-history-gui!
  "Reflect current undo/redo state into GUI."
  []
  (let [^JMenuItem mi-undo (.mi-undo @*gui*)
        ^JMenuItem mi-redo (.mi-redo @*gui*)]
    (if (hist/undo? @*history*)
      (doto mi-undo
        (.setEnabled true)
        (.setText (str "Undo " (.act (hist/current @*history*)))))
      (doto mi-undo
        (.setEnabled false)
        (.setText "Nothing to undo")))
    (if (hist/redo? @*history*)
      (doto mi-redo
        (.setEnabled true)
        (.setText (str "Redo " (.act (hist/peek-future @*history*)))))
      (doto mi-redo
        (.setEnabled false)
        (.setText "Nothing to redo")))))

(defn save-action!
  "Save the current state as a new history state. The action name will be
   displayed to the user as the name of the action that produced this state."
  [^String actname]
  (dosync
   (assoc-in-ref! *udata* [:act] actname)
   (ref-set *history* (hist/act @*history* @*udata*))
   (dirty! :history-gui)))

(defn cancel-action!
  "Cancel any in-progress commands and temporary state."
  []
  (dosync
   (assoc-in-ref! *state* [:drag-vertex] nil)
   (ref-set *udata* (hist/current @*history*))
   (dirty! :udata)))

;;;-- Math --;;;

;;;TODO: add {set,nudge}-{rotation,zoom,translation-{x,y}}! functions
;;;TODO: add functions to apply transforms to collections of coords

(defn ^Path2D view-cubic-curve
  "Calculate the path based on the current control points."
  [^Point2D p0, ^Point2D p1, ^Point2D p2, ^Point2D p3]
  (let [^Path2D path (Path2D$Double.)]
    (.moveTo path (.getX p0) (.getY p0))
    (.curveTo path
              (.getX p1) (.getY p1)
              (.getX p2) (.getY p2)
              (.getX p3) (.getY p3))
    (.createTransformedShape
     ^AffineTransform (.xform-to-view ^Viewpoint @*view*) path)))

(defn ^Point2D transform
  "Transform a location from one coordinate system to another."
  ([^AffineTransform at, sx, sy]
     (.transform at (Point2D$Double. sx sy) nil))
  ([^AffineTransform at, ^Point2D p]
     (.transform at p nil)))

(defn ^Point2D loc-to-view
  "Transform a location from world to viewport coords."
  [& args]
  (apply transform (.xform-to-view ^Viewpoint @*view*) args))

(defn ^Point2D loc-from-view
  "Transform a location from viewport to world coords."
  [& args]
  (apply transform (.xform-from-view ^Viewpoint @*view*) args))

(defn poly-len
  "Calculate the line length of a polyline (2+ vertices) of Point2Ds."
  [points]
  (apply + (interpolate-1 #(.distance ^Point2D %1 ^Point2D %2) points)))

(defn poly-bounds
  "Calculate the bounding Rectangle2D of a polyline (1+ vertices) in its native coordinate frame."
  [points]
  (let [xs (map #(.getX ^Point2D %) points)
        ys (map #(.getY ^Point2D %) points)
        xmin (apply min xs)
        xmax (apply max xs)
        ymin (apply min ys)
        ymax (apply max ys)]
    (Rectangle2D$Double. xmin ymin (- xmax xmin) (- ymax ymin))))

(defn poly-foldness
  "Compute the degree to which a polyline (2+ vertices) is folded up in
   its bounding box."
  [points]
  #_1 ; constant factor does not work -- gets choppy with >10 vertices
  #_(Math/sqrt (count points)) ; gets choppy a bit more slowly
  (let [bounds ^Rectangle2D (poly-bounds points)
        epsilon 0.001] ; a small value is added to prevent div by zero
    (/ (poly-len points)
       (+ (.width bounds) (.height bounds) epsilon)) ; much better, though should be used with a higher multiplier
    ))

(def ^{:doc "Radius (in view) for picking a point."} pick-radius 3)

(defn pick-vertex?
  "Determine if the (view) cursor is within a square picking radius of
   a (view) vertex."
  ([vx, vy, ^Point2D vvertex]
     (and (<= (Math/abs ^Double (- (.getX vvertex) vx)) pick-radius)
          (<= (Math/abs ^Double (- (.getY vvertex) vy)) pick-radius))))

(defn update-hover!
  "Check which vertex is hovered."
  []
  (dosync
   (let [curX (.mouseX ^ProgState @*state*)
         curY (.mouseY ^ProgState @*state*)
         hovered (first (filter #(pick-vertex? curX curY (loc-to-view %))
                                (.curve ^UserData @*udata*)))]
     (assoc-in-ref! *state* [:hover-vertex] hovered))))

;;;-- Rendering --;;;

(defn test-draw-point
  "Draw a test point at the given coords."
  [^Graphics2D g, ^Color c, wx, wy]
  (let [[vx vy] (de-pt (loc-to-view wx wy))]
    (doto g
      (.setPaint c)
      (.fill (Rectangle2D$Double. (- vx 3) (- vy 3) 6 6)))))

(def ^Color curve-color Color/RED)
(def ^BasicStroke curve-stroke
  (BasicStroke. (float 2.5) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))

(defn draw-curve
  "Draw a potentially incomplete curve."
  [^Graphics2D g, wpoints]
  (when (show-control-poly?)
    (draw-control-segments g (map loc-to-view wpoints))
    (draw-control-points g wpoints
                         (.xform-to-view ^Viewpoint @*view*)
                         (.hover-vertex ^ProgState @*state*)))
  (when (at-least-cubic?)
    (.setColor g curve-color)
    (.setStroke g curve-stroke)
    (let [smin 20
          smax 200
          smult 100
          samples (max smin (min smax (int (* (poly-foldness wpoints) smult))))]
      (.draw g (.createTransformedShape (.xform-to-view ^Viewpoint @*view*)
                                        (de-casteljau wpoints samples))))))

(defn render
  "Draw the world."
  [^Graphics2D g]
  (let [[w h] (de-dim (.viewport-dim ^Viewpoint @*view*))
        [cx cy] (de-pt (.view-center ^Viewpoint @*view*))]
    (doto g
      (.setRenderingHint RenderingHints/KEY_ANTIALIASING
                         RenderingHints/VALUE_ANTIALIAS_ON)
      (.setColor (Color. 50 50 50))
      (.fill (Rectangle2D$Double. 0 0 w h))))
  (draw-curve g (.curve ^UserData @*udata*)))

;;;-- GUI --;;;

(defn ask-redraw
  "Ask for the canvas to be redrawn."
  []
  (.repaint (.canvas ^GUI @*gui*)))

(defn update-mode!
  "Determine mode from user data state."
  []
  (dosync
   (assoc-in-ref! *state* [:mode]
                  (if (< (count (.curve ^UserData @*udata*)) 4)
                    :extend0
                    :extend1))))

(defn reflect-mode!
  "Update mode-dependent GUI elements."
  []
  (.setEnabled (.button-fit ^GUI @*gui*) (curve-has-extent?))
  (.setEnabled (.mi-view-control ^GUI @*gui*) (at-least-cubic?)))

;;;-- Event handlers --;;;

(defn do-history!
  "If (hist-when *history*) returns true, uses (hist-mod *history*) to get
   a new history object. Any active commands will be canceled, and the new
   current history item will overwrite udata."
  [hist-when hist-mod]
  (dosync
   (when (hist-when @*history*)
     (cancel-action!)
     (assoc-in-ref! *history* [] (hist-mod @*history*))
     (ref-set *udata* (hist/current @*history*))
     (dirty! :history-gui :udata))))

(defn calc-best-fit
  "Calculate new {:rot-center, :view-minspect} that may be merged into view."
  []
  (let [vverts (map loc-to-view (.curve @*udata*))
        ^Rectangle2D vbounds (poly-bounds vverts)
        ^Point2D wcenter (loc-from-view (.getCenterX vbounds)
                                        (.getCenterY vbounds))
        width-rescale (/ (.width vbounds)
                         (.getWidth (.canvas @*gui*)))
        height-rescale (/ (.height vbounds)
                          (.getHeight (.canvas @*gui*)))
        scale (max width-rescale height-rescale)
        new-minspect (* 1.08 (.view-minspect @*view*) scale)]
    {:rot-center wcenter :view-minspect new-minspect}))

(defn do-best-fit
  "If possible, bring the view into best fit around the curve."
  []
  (dosync
   (when (curve-has-extent?)
     (let [best (calc-best-fit)]
       (assoc-in-ref! *view* [:rot-center] (:rot-center best))
       (.setValue (.spinner-zoom @*gui*) (minspect-to-zoom (:view-minspect best)))
       (dirty! :pose :pose-spinners)))))

(defn do-maybe-exit
  "Exit, or possible ask user to save data first."
  []
  (.dispose (.frame @*gui*)))

(defn register-mouse-loc!
  "Make note of new mouse location, dirty if changed."
  ([x y]
     (dosync
      (when (or (assoc-in-ref! *state* [:mouseX] x)
                (assoc-in-ref! *state* [:mouseY] y))
        (dirty! :mouse-pos))))
  ([^MouseEvent e]
     (register-mouse-loc! (.getX e) (.getY e))))

;;;-- Event dispatch --;;;

;;; clean! will be called by relying code

(defn canvas-mouse-clicked
  "A click event has occurred on the canvas."
  [^MouseEvent e]
  (register-mouse-loc! e)
  (dosync
   (when (and (= (.getButton e) MouseEvent/BUTTON1)
              (not (.isShiftDown e))
              (not (.isControlDown e))
              (show-control-poly?)
              (not= (.mode @*state*) :manipulate))
     (append-vertex! (loc-from-view (.getX e) (.getY e)))
     (save-action! "add vertex"))))

(defn canvas-mouse-moved
  [^MouseEvent e]
  (register-mouse-loc! e))

(defn canvas-mouse-pressed
  "Might be the start of a drag."
  [^MouseEvent e]
  (register-mouse-loc! e) ; TODO cancel any in-progress dragging?
  (clean! :hover)
  (when (show-control-poly?)
    (dosync
     (assoc-in-ref! *state* [:drag-vertex] (.hover-vertex @*state*)))))

(defn canvas-mouse-dragged
  [^MouseEvent e]
  (register-mouse-loc! e)
  (when (show-control-poly?)
    (dosync
     (let [old-drag (.drag-vertex @*state*)]
       (if (nil? old-drag)
         (do
           (clean! :hover)
           (assoc-in-ref! *state* [:drag-vertex] (.hover-vertex @*state*)))
         (do
           (let [new-drag (loc-from-view (Point2D$Double. (.getX e) (.getY e)))]
             (assoc-in-ref! *state* [:drag-vertex] new-drag)
             (assoc-in-ref! *state* [:hover-vertex] new-drag)
             (replace-vertex! old-drag new-drag))))))))

(defn canvas-mouse-released
  "In some cases the end of a drag."
  [^MouseEvent e]
  (register-mouse-loc! e)
  (when-not (nil? (.drag-vertex @*state*))
    (save-action! "drag vertex")
    (assoc-in-ref! *state* [:drag-vertex] nil)
    (dirty! :painting))) ;TODO draw ghost of original during drag

(defn canvas-mouse-exited
  []
  (dosync
   (assoc-in-ref! *state* [:mouseX] -1)
   (assoc-in-ref! *state* [:mouseY] -1)
   (assoc-in-ref! *state* [:hover-vertex] nil)
   (dirty! :hover)))

(defn canvas-wheel-moved
  [^MouseWheelEvent e]
  (register-mouse-loc! e)
  );TODO: scroll wheel zooms by adjusting zoom spinner

;;;-- Components --;;;

(defn enliven!
  "Add action listeners to GUI components."
  [rgui]
  (doto (.mi-undo @rgui)
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [_]
         (do-history! hist/undo? hist/undo)
         (clean!)))))
  (doto (.mi-redo @rgui)
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [_]
         (do-history! hist/redo? hist/redo)
         (clean!)))))
  (doto (.mi-view-control @rgui)
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [_]
         (cancel-action!)
         (clean! :painting)))))
  (doto (.mi-exit @rgui)
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [_]
         (do-maybe-exit)))))
  (doto (.spinner-rot @rgui)
    (.addChangeListener
     (proxy [ChangeListener] []
       (stateChanged [_]
         (clean! :pose-spinners)))))
  (doto (.button-fit @rgui)
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [_]
         (do-best-fit)
         (clean!)))))
  (doto (.spinner-zoom @rgui)
    (.addChangeListener
     (proxy [ChangeListener] []
       (stateChanged [_]
         (clean! :pose-spinners)))))
  (let [mouse (proxy [MouseAdapter] []
                (mousePressed [e] (canvas-mouse-pressed e) (clean!))
                (mouseDragged [e] (canvas-mouse-dragged e) (clean!))
                (mouseReleased [e] (canvas-mouse-released e) (clean!))
                (mouseExited [_] (canvas-mouse-exited) (clean!))
                (mouseClicked [e] (canvas-mouse-clicked e) (clean!))
                (mouseMoved [e] (canvas-mouse-moved e) (clean!))
                (mouseWheelMoved [e] (canvas-wheel-moved e) (clean!)))]
    (doto (.canvas @rgui)
      (.addMouseListener mouse)
      (.addMouseMotionListener mouse)
      (.addMouseWheelListener mouse)
      (.addComponentListener
       (proxy [ComponentAdapter] []
         (componentResized [_]
           (clean! :canvas-shape)))))))

;;;-- Setup --;;;

(defn make-cascade
  "Create an initial state cascade."
  []
  (dirt/create :canvas-shape nil false ; shape and size of canvas component
               :center update-canvas-shape! [:canvas-shape]
               :pose-spinners nil false ; data in JSpinners
               :pose update-pose! [:pose-spinners]
               :mouse-pos nil false ; mouseX and mouseY in @*state*
               :hover update-hover! [:mouse-pos]
               :udata nil false ; pretty much anything in @*udata*
               :xform update-xform! [:pose :center]
               :mode update-mode! [:udata]
               :toolstate reflect-mode! [:mode]
               :history-gui update-history-gui! true ; undo/redo has been committed
               ;;; top-level states
               :painting ask-redraw [:udata :xform :hover]
               :gui nil [:toolstate :history-gui]
               ;;; collector
               :all nil [:painting :gui]
               ))

(defn launch
  "Create and display the GUI."
  []
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (dosync
   (ref-set *state* (make-blank-ProgState))
   (ref-set *gui* (make-blank-GUI)))
  (let [frame (create! *gui* [:frame] new-frame *gui* render)]
    (dosync
     (ref-set *view*
              (merge (make-blank-Viewpoint)
                     {:rot-center default-rot-center
                      :view-minspect default-view-minspect
                      :view-rot default-view-rot}))
     (ref-set *udata* (make-blank-UserData))
     (ref-set *cascade* (make-cascade))
     (ref-set *history* (hist/create @*udata*)))
    (clean!)
    (enliven! *gui*)
    (.setVisible frame true)))

(defn -main
  "Start application. Takes no arguments."
  [& args]
  (SwingUtilities/invokeLater launch))

