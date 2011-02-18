(ns timmcHW3.gui
   "Interface constructors."
   (:use timmcHW3.utils)
   (:require timmcHW3.state)
   (:import [timmcHW3.state GUI])
   (:import
      [java.awt Dimension Graphics2D Component BorderLayout]
      [javax.swing BoxLayout BorderFactory
         JFrame JComponent JPanel JMenu JMenuBar JMenuItem JCheckBoxMenuItem JButton JSpinner SpinnerNumberModel JSpinner$NumberEditor
         KeyStroke]
      [javax.swing.border Border EtchedBorder]))

; We define these in functions so they are not created at compile-time.

(defn ^JMenuItem new-mi-undo
   [rgui]
   (doto (JMenuItem. "Undo")
      (.setEnabled false)
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Z"))))

(defn ^JMenuItem new-mi-redo
   [rgui]
   (doto (JMenuItem. "Redo")
      (.setEnabled false)
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Y"))))

(defn ^JMenuItem new-mi-exit
   [rgui]
   (doto (JMenuItem. "Exit")
      (.setAccelerator (KeyStroke/getKeyStroke "ctrl Q"))))

(defn ^JCheckBoxMenuItem new-mi-view-control
   [rgui]
   (JCheckBoxMenuItem. "Show control polygon" true))

(defn ^JMenuBar new-menu
   "Make a menu bar."
   [rgui]
   (doto (JMenuBar.)
      (.add (doto (JMenu. "Spline")
               (.add (create! rgui [:mi-undo] new-mi-undo rgui))
               (.add (create! rgui [:mi-redo] new-mi-redo rgui))
               (.add (create! rgui [:mi-exit] new-mi-exit rgui))))
      (.add (doto (JMenu. "View")
               (.add (create! rgui [:mi-view-control] new-mi-view-control rgui))))))

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
   [rgui]
   (let [nm (SpinnerNumberModel. 0.0 nil nil 0.03) ; radians!
         js (JSpinner. nm)
         ned (JSpinner$NumberEditor. js "#####0.000")]
      (-> ned (.getTextField) (.setColumns 5))
      (.setEditor js ned)
      js))

(defn ^JSpinner new-pose-zoom
   "Make the spinner for viewpoint zooming."
   [rgui]
   (let [nm (SpinnerNumberModel. 1.0 0.001 20.0 0.003) ; log scale, higher is greater mag
         js (JSpinner. nm)
         ned (JSpinner$NumberEditor. js "#####0.000")]
      (-> ned (.getTextField) (.setColumns 5))
      (.setEditor js ned)
      js))

(defn ^JPanel new-pose-panel
   "Make a Pose panel."
   [rgui]
   (let [p (JPanel.)]
      (doto p
         (.setBorder (make-controls-border "Position"))
         (.add (create! rgui [:spinner-rot] new-pose-rotate rgui))
         (.add (create! rgui [:spinner-zoom] new-pose-zoom rgui)))))

(defn ^JPanel new-controls
   "Make a control panel."
   [rgui]
   (let [p (JPanel.)]
      (doto p
         (.add (make-vertical-layout
                  (new-pose-panel rgui))))))

(defn ^JComponent new-canvas
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

