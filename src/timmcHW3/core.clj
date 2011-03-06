(ns timmcHW3.core
  "Core code. Use -main."
  (:use [timmcHW3.utils])
  (:import [timmcHW3.utils Vec2])
  (:use [timmcHW3.drawing])
  (:use [timmcHW3.gui])
  (:import [timmcHW3.gui GUI])
  (:require [timmcHW3.user-data :as udata])
  (:import [timmcHW3.user-data UserData])
  (:use timmcHW3.state)
  (:import [timmcHW3.state Viewpoint ProgState])
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
   [java.awt.event ComponentAdapter
    MouseAdapter MouseEvent MouseMotionAdapter MouseWheelListener])
  (:gen-class))

(def ^{:doc "Collection of GUI components."} *gui* (ref nil))
(def ^{:doc "The viewpoint's state."} *view* (ref nil))
(def ^{:doc "Tool and activity state."} *state* (ref nil))
(def ^{:doc "User data that needs undo/redo."} *udata* (ref nil))
(def ^{:doc "Undo/redo buffers."} *history* (ref nil))
(def ^{:doc "State dirtiness cascade."} *cascade* (ref nil))

(defn dirty!
  "Mark one or more pieces of state as dirty."
  [& kws]
  (dosync
   (alter *cascade* (partial reduce dirt/dirty) kws)
   nil))

(defn clean!
  "Clean up state and re-display where necessary."
  ([& kws]
     (apply dirty! kws)
     (clean!))
  ([]
     (dosync
      (alter *cascade* dirt/clean :all)
      nil)))

;;;-- Mode accessors --;;;

(defn show-control-poly?
  "Check whether we are showing the control polygon."
  []
  (or (.getState (.mi-view-control @*gui*))
      (not (udata/curve-cubic+? @*udata*))))

;;;-- Viewpoint --;;;

(def default-rot-center (pt 0 0))
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
     (rassoc *view* [:xform-to-view] at)
     (rassoc *view* [:xform-from-view] (.createInverse at)))))

(defn update-canvas-shape!
  "Update variables that depend on the canvas size or other state."
  []
  (dosync
   (let [dim (.getSize (.canvas @*gui*))
         [dim-w dim-h] (de-dim dim)]
     (rassoc *view* [:view-center] (pt (/ dim-w 2) (/ dim-h 2)))
     (rassoc *view* [:viewport-dim] dim))))

(defn update-pose!
  "Update view based on current state of Pose spinners."
  []
  (dosync
   (let [rot (double (.getValue (.spinner-rot @*gui*)))
         minspect (zoom-to-minspect (.getValue (.spinner-zoom @*gui*)))]
     (alter *view* assoc :view-rot rot)
     (alter *view* assoc :view-minspect minspect))))

;;;-- User data modifiers --;;;

(defn append-vertex!
  "Add a vertex to the curve."
  [^Point2D wp]
  (dosync
   (rupdate *udata* [:curve] conj wp)
   (dirty! :udata)))

(defn replace-1
  "Replace the first x in coll such that (pred x) is true with (gen x).
   Not tail-recursive."
  [coll pred gen]
  (if (seq coll)
    (if (pred (first coll))
      (conj (rest coll)
            (gen (first coll)))
      (conj (replace-1 (rest coll) pred gen)
            (first coll)))
    coll))

(defn replace-vertex!
  "Replace an old world-vertex with a new one, mark dirty."
  [^Point2D old,^Point2D new]
  (when (nil? new)
    (throw (IllegalArgumentException. "Given nil replacement vertex.")))
  (dosync
   ;;; Might consider (replace (IdentityHashMap. {d0 d1}) [3 4 d0 5])
   ;;; NB: Ensure that result is a vector, and not reversed.
   (rupdate *udata* [:curve]
            (comp vec replace-1) (partial identical? old) (constantly new))
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
   (rassoc *udata* [:act] actname)
   (alter *history* hist/act @*udata*)
   (dirty! :history-gui)))

(defn cancel-action!
  "Cancel any in-progress commands and temporary state."
  []
  (dosync
   (rassoc *state* [:drag-vertex] nil)
   (ref-set *udata* (hist/current @*history*))
   (dirty! :udata)))

;;;-- Math --;;;

(defn ^AffineTransform to-view [] (.xform-to-view ^Viewpoint @*view*))
(defn ^AffineTransform from-view [] (.xform-from-view ^Viewpoint @*view*))

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
    (.createTransformedShape (to-view) path)))

(defmulti xform
  "Transform using given coordinates."
  (fn [_ x] (class x)))
(defmethod ^Point2D xform Point2D
  [^AffineTransform at, ^Point2D p]
  (.transform at p nil))
(defmethod ^Vec2 xform Vec2
  [^AffineTransform at, ^Vec2 v]
  (fvec2<pt #(.deltaTransform at % nil)))

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
  ;; constant factor does not work -- gets choppy with >10 vertices
  #_1
  ;; gets choppy a bit more slowly
  #_(Math/sqrt (count points))
  ;; much better, though should be used with a higher multiplier
  (let [bounds ^Rectangle2D (poly-bounds points)
        epsilon 0.001] ; a small value is added to prevent div by zero
    (/ (poly-len points)
       (+ (.width bounds) (.height bounds) epsilon))
    ))

(def ^{:doc "Radius (in view) for picking a point."} pick-radius 5)

(defn pick-vertex?
  "Determine if the (view) cursor is within a square picking radius of
   a (view) vertex."
  ([vx, vy, ^Point2D vvertex]
     (and (<= (Math/abs ^Double (- (.getX vvertex) vx)) pick-radius)
          (<= (Math/abs ^Double (- (.getY vvertex) vy)) pick-radius))))

(defn ^Point2D calc-hover
  "Return the hovered vertex or nil."
  [^Point2D vpos, vertices]
  (let [[vx vy] (de-pt vpos)]
    (first (filter #(pick-vertex? vx vy (xform (to-view) %))
                   (rseq vertices)))))

(defn update-hover!
  "Check which vertex is hovered, starting with latest."
  []
  (dosync
   (rassoc *state* [:hover-vertex]
           (if-let [mouse-pos (.mouse-pos ^ProgState @*state*)]
             (and (not (.drag-viewpoint? @*state*))
                  (or (.drag-vertex @*state*)
                      (calc-hover mouse-pos (.curve ^UserData @*udata*))))
             nil))))

;;;-- Rendering --;;;

(def ^Color curve-color Color/RED)
(def ^BasicStroke curve-stroke
  (BasicStroke. (float 2.5) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))

(defn draw-curve
  "Draw a potentially incomplete curve."
  [^Graphics2D g, wpoints]
  (when (show-control-poly?)
    (let [vpoints (map (partial xform (to-view)) wpoints)]
      (draw-control-segments g vpoints)
      (draw-control-points g vpoints)
      (when-let [hover (.hover-vertex ^ProgState @*state*)]
        (draw-hover g (xform (to-view) hover)))))
  (when (udata/curve-cubic+? @*udata*)
    (.setColor g curve-color)
    (.setStroke g curve-stroke)
    (let [smin 20
          smax 200
          smult 100
          samples (max smin (min smax (int (* (poly-foldness wpoints) smult))))]
      (.draw g (.createTransformedShape (to-view)
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
   (rassoc *state* [:mode]
           (if (udata/curve-cubic+? @*udata*)
             :extend1
             :extend0))))

(defn reflect-mode!
  "Update mode-dependent GUI elements."
  []
  (.setEnabled (.mi-clear ^GUI @*gui*)
               (udata/has-data? @*udata*))
  (.setEnabled (.button-fit ^GUI @*gui*)
               (udata/curve-extent? @*udata*))
  (.setEnabled (.mi-view-control ^GUI @*gui*)
               (udata/curve-cubic+? @*udata*)))

;;;-- Event handlers --;;;

(defn do-clear
  "Clear all user data."
  []
  (dosync
   (cancel-action!)
   (rassoc *udata* [:curve] [])
   (save-action! "clear")))

(defn do-history!
  "If (hist-when *history*) returns true, uses (hist-mod *history*) to get
   a new history object. Any active commands will be canceled, and the new
   current history item will overwrite udata."
  [hist-when hist-mod]
  (dosync
   (when (hist-when @*history*)
     (cancel-action!)
     (alter *history* hist-mod)
     (ref-set *udata* (hist/current @*history*))
     (dirty! :history-gui :udata))))

(defn calc-best-fit
  "Calculate new {:rot-center, :view-minspect} that may be merged into view."
  []
  (let [vverts (map (partial xform (to-view)) (.curve @*udata*))
        ^Rectangle2D vbounds (poly-bounds vverts)
        ^Point2D wcenter (xform (from-view) (pt (.getCenterX vbounds)
                                                (.getCenterY vbounds)))
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
  (->>
   (dosync
    (when (udata/curve-extent? @*udata*)
      (let [best (calc-best-fit)]
        (rassoc *view* [:rot-center] (:rot-center best))
        (dirty! :pose :pose-spinners)
        (minspect-to-zoom (:view-minspect best)))))
   (set-number-spinner (.spinner-zoom @*gui*))))

(defn do-maybe-exit
  "Exit, or possible ask user to save data first."
  []
  (.dispose (.frame @*gui*)))

(defn register-mouse-loc!
  "Make note of new mouse location, dirty if changed."
  ([x y]
     (dosync
      (when (rassoc *state* [:mouse-pos] (pt x y))
        (dirty! :mouse-pos))))
  ([^Point2D p]
     (apply register-mouse-loc! (de-pt p))))

;;;-- Event dispatch --;;;

;;; clean! will be called by relying code

(defmulti ^Point2D loc "Get view location." class)
(defmethod loc Point2D [p] p)
(defmethod loc MouseEvent [e] (pt (.getX e) (.getY e)))

(defn canvas-mouse-clicked
  "A click event has occurred on the canvas."
  [^MouseEvent e]
  (register-mouse-loc! (loc e))
  (dosync
   (when (and (= (.getButton e) MouseEvent/BUTTON1)
              (not (.isShiftDown e))
              (not (.isControlDown e))
              (show-control-poly?)
              (not= (.mode @*state*) :manipulate))
     (append-vertex! (xform (from-view) (loc e)))
     (save-action! "add vertex")
     (dirty! :hover))))

(defn canvas-mouse-moved
  [^MouseEvent e]
  (register-mouse-loc! (loc e)))

(defn canvas-mouse-pressed
  "Might be the start of a drag."
  [^MouseEvent e]
  (register-mouse-loc! (loc e))
  (clean! :hover)
  (dosync
   (let [can-modify (show-control-poly?)
         hovered (.hover-vertex @*state*)]
     (if (and can-modify hovered)
       (rassoc *state* [:drag-vertex] hovered)
       (rassoc *state* [:drag-viewpoint?] true)))))

(defn fworld<view ; make this a multimethod of to-view?
  "Transform a view-space function into a world-space function."
  [fv & args]
  (isomorph #(apply fv % args)
            (partial xform (to-view))
            (partial xform (from-view))))

(defn canvas-mouse-dragged
  [^MouseEvent e]
  (dosync
   ;; FIXME: The delta approach can slowly lead to noticeable drifting of cursor on extreme zoom
   (let [vdelta (pt-diff (.mouse-pos @*state*) (loc e))
         can-modify (show-control-poly?)]
     (register-mouse-loc! (loc e))
     (if-let [old-drag (.drag-vertex @*state*)]
       (when can-modify
         (let [new-drag ((fworld<view pt+ vdelta) old-drag)]
           (rassoc *state* [:drag-vertex] new-drag)
           (replace-vertex! old-drag new-drag)))
       (do
         (when can-modify ; don't bother otherwise
           (clean! :hover))
         (let [hover (.hover-vertex @*state*)
               drag-view? (.drag-viewpoint? @*state*)]
           (if (and (not drag-view?) can-modify hover)
             (do (rassoc *state* [:drag-vertex] hover)
                 (dirty! :mode))
             (do (rassoc *view* [:rot-center]
                         ((fworld<view pt+ (vec-neg vdelta)) (.rot-center @*view*)))
                 (rassoc *state* [:drag-viewpoint?] true) ; remain or become true
                 (dirty! :pose)))))))))

(defn canvas-mouse-released
  "In some cases the end of a drag."
  [^MouseEvent e]
  (register-mouse-loc! (loc e))
  (dosync
   (when-not (nil? (.drag-vertex @*state*))
     (save-action! "drag vertex")
     (rassoc *state* [:drag-vertex] nil)
     (dirty! :painting)) ;TODO draw ghost of original during drag
   (rassoc *state* [:drag-viewpoint?] false)))

(defn canvas-mouse-exited
  []
  (dosync
   (rassoc *state* [:hover-vertex] nil)
   (dirty! :hover)))

(defn canvas-wheel-moved
  [^MouseWheelEvent e]
  (register-mouse-loc! (loc e))
  );TODO: scroll wheel zooms by adjusting zoom spinner

;;;-- Components --;;;

(defn enliven!
  "Add action listeners to GUI components."
  [rgui]
  (let [handlers
        {:mi-clear        #(do-clear)
         :mi-undo         #(do-history! hist/undo? hist/undo)
         :mi-redo         #(do-history! hist/redo? hist/redo)
         :mi-view-control #(do (cancel-action!)
                               (dirty! :painting))
         :mi-exit         #(do-maybe-exit)
         :button-fit      #(do-best-fit)
        }]
    (doseq [[k f] handlers]
      (add-action-handler (k @rgui)
                          #(do (f) (clean!)))))
  (doto (.spinner-rot @rgui)
    (.addChangeListener
     (proxy [ChangeListener] []
       (stateChanged [_]
         (clean! :pose-spinners)))))
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
               :mouse-pos nil false ; mouse-pos in @*state*
               :udata nil false ; pretty much anything in @*udata*
               :hover update-hover! [:mouse-pos :udata]
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
     (ref-set *udata* (udata/create))
     (ref-set *cascade* (make-cascade))
     (ref-set *history* (hist/create @*udata*)))
    (clean!)
    (enliven! *gui*)
    (.setVisible frame true)))

(defn -main
  "Start application. Takes no arguments."
  [& args]
  (SwingUtilities/invokeLater launch))

