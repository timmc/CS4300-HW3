(ns timmcHW3.cascade
  "Manage state dirtiness dependency cascades using a persistent object.
  
   State update dependencies are modeled as a directed acyclic graph of nodes
   that can be clean or dirty, where setting a node as dirty turns the nodes
   at all outedges dirty recursively.
   Each node is associated with a \"cleaner\" function intended to update
   that aspect of the relying program's state. When a dependant node needs
   to be marked clean, it cleans all of its own dependencies and then calls
   its own cleaner function. There is no strict guarantee that a node's
   cleaner will not be called multiple times in case of a diamond in the
   graph, so cleaners must be idempotent. (However, the cleanup algorithm
   does try to avoid this scenario as a performance measure.)

   Recommended usage: (create ...) a cascade, specifying all nodes at once,
   and save it in a ref. Use (dirty ...) whenever a piece of your program's
   state has changed, and (clean ...) on that node in order to force it
   to become clean again. clean is a constant-time operation on clean nodes.

   Copyright 2011 Tim McCormack; free-licensed under GPL v3."
  (:require [clojure.set :as set])
  (:gen-class))

; Implementation details:
; - Top object is a map
; - Keys of map are keywords representing nodes of state
; - Values of map are themselves maps:
;   - :clean? <boolean> - whether this node is clean
;   - :deps <set<keyword>>} - nodes that this node depends on
;   - :cleaner <fn> - the cleaner for this node of the relying program's state

(defn clean?
  "Check if a node is clean."
  [cascade node-kw]
  (-> cascade node-kw :clean?))

(defn add
  "Given a cascade, return a new cascade with the new node, cleaner function,
   and either a list of dependencies or the current cleanliness state.
   The dependency nodes must already exist, and the new node must not.
   The cleaner is expected to be a nullary function and will only be called
   when the dependencies are clean.
   If there are no dependencies, the initial boolean state must be given instead.
   Otherwise, initial state is computed by conjunction of the states of
   dependent nodes."
  [cascade node-kw cleaner-fn req-kws-or-clean?]
  (when (contains? cascade node-kw)
    (throw (IllegalArgumentException. (str "Node keyword already exists in cascade:" node-kw))))
  (when (coll? req-kws-or-clean?)
    (when (empty? req-kws-or-clean?)
      (throw (IllegalArgumentException. "Dependency list must be non-empty (or be replaced by initial state).")))
    (doseq [d req-kws-or-clean?]
      (when-not (contains? cascade d)
	(throw (IllegalArgumentException. (str "Dependency keyword does not exist in cascade:" d))))))
  (let [[initial deps] (if (coll? req-kws-or-clean?)
                         [(every? true? (map #(clean? cascade %)
					     req-kws-or-clean?))
			  (set req-kws-or-clean?)]
                         [req-kws-or-clean?
			  #{}])]
    (assoc cascade node-kw {:clean? initial :deps deps :cleaner cleaner-fn})))

(defn create
  "Create a cascade manager and initialize by add'ing each triplet of arguments
   in order."
  [& adds]
  (when-not (zero? (mod (count adds) 3))
    (throw (IllegalArgumentException.
	    "Must provide triplets of arguments to cascade creator.")))
  (reduce #(apply add %1 %2) {} (partition 3 adds)))

(defn cleaner
  "Get the thunk that will be called to clean this node's program state."
  [cascade node-kw]
  (-> cascade node-kw :cleaner))

(defn dependencies-1
  "Return the set of node keywords that this node depends immediately upon."
  [cascade node-kw]
  (-> cascade node-kw :deps))

(defn dependencies
  "Return the full set of node keywords that this node depends upon."
  [cascade node-kw]
  (apply set/union (for [d (dependencies-1 cascade node-kw)]
		     (conj (dependencies cascade d) d))))

(defn set-all
  "Set all nodes to the given state (true = clean)."
  [cascade state]
  (into {} (for [[k vm] cascade]
	     [k (assoc vm :clean? state)])))

(defn- set-single
  "Set a single node to the given state *without* propagating."
  [c n s]
  (assoc-in c [n :clean?] s))

(defn dependants-1
  "Provide the set of immediate dependants of a node."
  [c n]
  (set (filter (fn [k] (some #(= n %)
			     (-> c k :deps)))
	       (keys c))))

(defn dependants
  "Provide set of all eventual dependents of a node."
  [c n]
  (loop [accum #{}
	 worklist #{n}]
    (let [next-layer (apply set/union (map (partial dependants-1 c)
					   worklist))]
      (if (seq next-layer)
	(recur (set/union accum next-layer) next-layer)
	accum))))

(defn- dirty-set
  "Dirty all the nodes in the given set."
  [c ns]
  (if (empty? ns)
    c
    (let [n (first ns)]
      (if (clean? c n)
	(recur (set-single c n false)
	       (into (rest ns) (dependants-1 c n)))
	(recur c (rest ns))))))

(defn dirty
  "Mark these nodes and all dependents dirty."
  [cascade & nodes]
  (dirty-set cascade (into #{} nodes)))

(defn to-clean
  "Return {:fns <coll<fn>> :nodes <set<keyword>>} where :fns is the sequence of
   nullary functions that must be successfully called to clean the node and
   its dependencies, and :nodes is the set of nodes that will be changed."
  [c n]
  (if (clean? c n)
    {:fns [] :nodes #{}}
    (let [dirty-parents (filter (complement (partial clean? c))
				(dependencies-1 c n))
	  parcleans (map (partial to-clean c) dirty-parents)
	  pfns (map :fns parcleans)
	  pnodes (map :nodes parcleans)]
      {:fns (concat (distinct (apply concat pfns))
		    [(cleaner c n)])
       :nodes (conj (apply set/union pnodes) n)})))

(defn clean
  "Run all cleaners necessary to get the specified node clean,
   and return updated cascade."
  [cascade node-kw]
  (let [{thunks :fns affected :nodes} (to-clean cascade node-kw)
	success (reduce #(set-single %1 %2 true) cascade affected)]
    (doseq [t thunks] (t))
    success))

