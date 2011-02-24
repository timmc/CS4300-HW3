(ns timmcHW3.state
  "State records."
  (:import
   [java.awt Dimension]
   [java.awt.geom AffineTransform Point2D]
   [javax.swing JFrame JComponent JPanel
    JMenu JMenuBar JMenuItem JCheckBoxMenuItem
    JSpinner JButton]))

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
   ^{:doc "Exit menu item." :tag JMenuItem}
   mi-exit
   ^{:doc "View control polygon toggle button." :tag JCheckBoxMenuItem}
   mi-view-control
   ^{:doc "Spinner for angle of rotation, in radians." :tag JSpinner}
   spinner-rot
   ^{:doc "Spinner for degree of zoom, using default zoom 1. Double mag by adding 0.1." :tag JSpinner}
   spinner-zoom
   ^{:doc "Button to zoom to extent of curve." :tag JButton}
   button-fit
   ])

(defn ^GUI make-blank-GUI
  []
  (GUI. nil nil nil nil nil nil nil nil nil nil nil))

(defrecord ^{:doc "Viewport state."}
    Viewpoint
  [^{:doc "Chosen center of rotation." :tag Point2D}
   rot-center ; Translation: User drags new world point to center of window.
   ^{:doc "Minimum extent of world to show in both width and height."}
   view-minspect ; Scale: Resizing the window stretches the view.
   ^{:doc "Rotation of viewport."}
   view-rot ; Rotation: Around the center of the window.
   ^{:doc "Viewport's pixel center coordinates as Point2D." :tag Point2D}
   view-center ; Centering: This is the center of rotation of the viewpoint.
   ^{:doc "World-to-viewport transform." :tag AffineTransform}
   xform-to-view
   ^{:doc "The inverse transform, viewport-to-world." :tag AffineTransform}
   xform-from-view
   ^{:doc "Dimensions of viewport." :tag Dimension}
   viewport-dim
   ])

(defn ^Viewpoint make-blank-Viewpoint
  []
  (Viewpoint. nil nil nil nil nil nil nil))

(defrecord ^{:doc "Whole-program state."}
    ProgState
  [^{:doc "Overall mode:
            :extend0 - Wait for sufficient input to define new curve. Allow vertex input.
            :extend1 - Wait for indication that new curve is done. Allow vertex input or manipulation.
            :manipulate - Allow dragging of vertices."}
   mode
   ^{:doc "Last mouse X position in view"}
   mouseX
   ^{:doc "Last mouse Y position in view"}
   mouseY
   ^{:doc "World-vertex (identical) that cursor is hovering over, or nil."}
   hover-vertex
   ^{:doc "World-vertex (identical) that is being dragged, or nil."}
   drag-vertex
   ])

(defn ^ProgState make-blank-ProgState
  []
  (ProgState. :extend0 -1 -1 nil nil))

(defrecord ^{:doc "Current state of user's data. This is saved in undo/redo buffers."}
    UserData
  [^{:doc "The act that produced this state, e.g. \"vertex drag\" or empty string."}
   act
   ^{:doc "Vector of Bézier curve control vertices Possibly empty."}
   curve
   ])

(defn ^UserData make-blank-UserData
  []
  (UserData. "Initialization" []))

