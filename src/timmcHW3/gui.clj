(ns timmcHW3.gui
  "Interface constructors."
  (:use timmcHW3.utils)
  (:require timmcHW3.state)
  (:import [timmcHW3.state GUI])
  (:import
   [java.awt Dimension Graphics2D Component BorderLayout]
   [javax.swing BoxLayout BorderFactory
    JFrame JComponent JPanel JMenu JMenuBar JMenuItem JCheckBoxMenuItem
    JButton JLabel JSpinner SpinnerNumberModel JSpinner$NumberEditor
    KeyStroke]
   [javax.swing.border Border EtchedBorder]))

;;;-- Utilities --;;;

(defn- ^Border make-controls-border
  [title]
  (-> (BorderFactory/createEtchedBorder EtchedBorder/LOWERED)
      (BorderFactory/createTitledBorder title)))

(defn- ^Component make-vertical-layout
  "Lay out a series of components as a vertical stack with an expanding south. Not tail-recursive."
  [^int vgap, ^Component c & more]
  (let [cont (JPanel.)]
    (.setLayout cont (BorderLayout. 0 vgap))
    (.add cont c BorderLayout/PAGE_START)
    (when (seq more)
      (.add cont ^Component (apply make-vertical-layout vgap more) BorderLayout/CENTER))
    cont))

(defn- ^Component add-label
  "Return a component with a label attached to the given component."
  [^String text, ^JComponent child]
  (doto (JPanel.)
    (.setLayout (BorderLayout. 4 0))
    (.add (JLabel. text) BorderLayout/LINE_START)
    (.add child BorderLayout/LINE_END)))

;;;-- Component creators --;;;

;;; We define these in functions so they are not created at compile-time.
;;; These methods each take a ref to a GUI record that they can add their components to.

(defn- ^JMenuItem new-mi-clear
  []
  (doto (JMenuItem. "Clear")
    (.setEnabled false)))

(defn- ^JMenuItem new-mi-undo
  []
  (doto (JMenuItem. "Undo")
    (.setEnabled false)
    (.setAccelerator (KeyStroke/getKeyStroke "ctrl Z"))))

(defn- ^JMenuItem new-mi-redo
  []
  (doto (JMenuItem. "Redo")
    (.setEnabled false)
    (.setAccelerator (KeyStroke/getKeyStroke "ctrl Y"))))

(defn- ^JMenuItem new-mi-exit
  []
  (doto (JMenuItem. "Exit")
    (.setAccelerator (KeyStroke/getKeyStroke "ctrl Q"))))

(defn- ^JCheckBoxMenuItem new-mi-view-control
  []
  (doto (JCheckBoxMenuItem. "Show control polygon" true)
    (.setAccelerator (KeyStroke/getKeyStroke "ctrl P"))))

(defn- ^JMenuBar new-menu
  "Make a menu bar."
  [rgui]
  (doto (JMenuBar.)
    (.add (doto (JMenu. "Spline")
            (.add (create! rgui [:mi-clear] new-mi-clear))
	    (.add (create! rgui [:mi-undo] new-mi-undo))
	    (.add (create! rgui [:mi-redo] new-mi-redo))
	    (.add (create! rgui [:mi-exit] new-mi-exit))))
    (.add (doto (JMenu. "View")
	    (.add (create! rgui [:mi-view-control] new-mi-view-control))))))

(defn- ^JSpinner new-pose-rotate
  "Make the spinner for viewpoint rotation."
  [rgui]
  (let [nm (SpinnerNumberModel. 0.0 nil nil 0.03) ; radians!
	js (JSpinner. nm)
	ned (JSpinner$NumberEditor. js "#####0.000")]
    (-> ned (.getTextField) (.setColumns 5))
    (.setEditor js ned)
    js))

(defn- ^JSpinner new-pose-zoom
  "Make the spinner for viewpoint zooming."
  [rgui]
  (let [nm (SpinnerNumberModel. 1.0 0.001 20.0 0.003) ; log scale, higher is greater mag
	js (JSpinner. nm)
	ned (JSpinner$NumberEditor. js "#####0.000")]
    (-> ned (.getTextField) (.setColumns 5))
    (.setEditor js ned)
    js))

(defn- ^JButton new-fit-button
  "Make a Best Fit button for centering the polygon."
  [rgui]
  (doto (JButton. "Best fit")
    (.setToolTipText "Zoom curve to just fit inside viewport")))

(defn- ^JPanel new-pose-panel
  "Make a Pose panel."
  [rgui]
  (doto (JPanel.)
    (.setBorder (make-controls-border "Position"))
    (.add (make-vertical-layout 4
				(add-label "Rotation:"
					   (create! rgui [:spinner-rot] new-pose-rotate rgui))
				(add-label "Zoom:"
					   (create! rgui [:spinner-zoom] new-pose-zoom rgui))
				(create! rgui [:button-fit] new-fit-button rgui)))))

(defn- ^JPanel new-controls
  "Make a control panel."
  [rgui]
  (let [p (JPanel.)]
    (doto p
      (.add (make-vertical-layout 0
				  (new-pose-panel rgui))))))

;;;TODO make focusable
(defn- ^JComponent new-canvas
  "Make a drawing canvas."
  [rgui renderer]
  (let [jc (proxy [JComponent] []
	     (paint [^Graphics2D g] (renderer g)))]
    (doto jc
      (.setDoubleBuffered true)
      (.setMinimumSize (Dimension. 10 10))
      (.setPreferredSize (Dimension. 600 600)))))

(defn ^JFrame new-frame
  "Make the application window."
  [rgui renderer]
  (let [fr (JFrame. "CS4300 HW3 - TimMc")]
    (doto fr
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setJMenuBar (create! rgui [:menu] new-menu rgui))
      (.setLayout (BorderLayout.))
      (.add (create! rgui [:controls] new-controls rgui) BorderLayout/LINE_START)
      (.add (create! rgui [:canvas] new-canvas rgui renderer) BorderLayout/CENTER)
      (.pack))))

;;;TODO add statusbar
