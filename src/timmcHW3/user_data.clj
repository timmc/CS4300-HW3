(ns timmcHW3.user-data
  "User's data. Snapshots may be stored in history buffers.")

(defrecord ^{:doc "Current state of user's data. This is saved in undo/redo buffers."}
    UserData
  [^{:doc "The act that produced this state, e.g. \"vertex drag\" or empty string."}
   act
   ^{:doc "Vector of Bézier curve control vertices Possibly empty."}
   curve
   ])

(defn ^UserData create
  []
  (UserData. "Initialization" []))

(defn has-data?
  "Return true if user data is not empty."
  [^UserData udata]
  (boolean (>= (count (.curve udata)) 1)))

(defn curve-extent?
  "Return true if we can define the size of the control polygon."
  [^UserData udata]
  (boolean (>= (count (.curve udata)) 2)))

(defn curve-cubic+?
  "Return true if we are showing enough points to draw a non-trivial curve."
  [^UserData udata]
  (boolean (>= (count (.curve udata)) 3)))

