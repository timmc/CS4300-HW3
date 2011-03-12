(ns timmcHW3.history
  "History manager (undo/redo) using persistent data structure.

   Intended use: Use public functions instead of directly using History object."
  (:gen-class))

;;;; Backend

(defrecord ^{:doc "Holds current state, undo stack, and redo stack."}
    History
  [^{:doc "Past actions. (Undo buffer.)"}
   past
   ^{:doc "Current state, probably annotated with action that produced it."}
   present
   ^{:doc "Actions that have been undone. (Redo buffer.)"}
   future
   ])

;; Stack semantics
(def push conj)

;;;; Constructors

(defn ^History create
  "Create an empty history buffer with given current state."
  [current]
  (History. () current ()))

;;;-- Accessors --;

(defn ^boolean undo?
  "Return true if undo is possible."
  [^History h]
  (boolean (seq (.past h))))

(defn ^boolean redo?
  "Return true if redo is possible."
  [^History h]
  (boolean (seq (.future h))))

(defn peek-past
  "Return the most recent past state, or nil if none."
  [^History h]
  (peek (.past h)))

(defn current
  "Return the current state."
  [^History h]
  (.present h))

(defn peek-future
  "Return the next future state, or nil if none."
  [^History h]
  (peek (.future h)))

;;;; Modifiers

(defn ^History undo
  "Undo if possible, else return input value."
  [^History h]
  (if (undo? h)
    (History. (pop (.past h))
	      (peek (.past h))
	      (push (.future h) (.present h)))
    h))

(defn ^History redo
  "Redo if possible, else return input value."
  [^History h]
  (if (redo? h)
    (History. (push (.past h) (.present h))
	      (peek (.future h))
	      (pop (.future h)))
    h))

(defn ^History act
  "Push an action state onto the history, dropping the redo buffer."
  [^History h, state]
  (History. (push (.past h) (.present h))
	    state
	    ()))

