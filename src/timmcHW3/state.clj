(ns timmcHW3.state
  "State records."
  (:import
   [java.awt Dimension]
   [java.awt.geom AffineTransform Point2D]))

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
   ^{:doc "Last mouse position in view, or nil." :tag Point2D}
   mouse-pos
   ^{:doc "World-vertex (identical) that cursor is hovering over, or nil." :tag Point2D}
   hover-vertex
   ^{:doc "World-vertex (identical) that is being dragged, or nil." :tag Point2D}
   drag-vertex
   ])

(defn ^ProgState make-blank-ProgState
  []
  (ProgState. :extend0 nil nil nil))
