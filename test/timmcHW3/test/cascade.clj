(ns timmcHW3.test.cascade
  (:use [timmcHW3.cascade] :reload)
  (:use [clojure.test]))

(deftest empty-create
  (is (empty? (create))))

(deftest explicit-state
  (is (= (clean? (create :foo #() true) :foo) true))
  (is (= (clean? (create :foo #() false) :foo) false)))

(def trimix
  (create :foo #() true
	  :bar #() false
	  :baz #() true
	  :qux #() [:foo :bar :baz]))

(deftest create-and-test
  (is (= (clean? trimix :foo) true))
  (is (= (clean? trimix :bar) false))
  (is (= (clean? trimix :baz) true))
  (is (= (clean? trimix :qux) false)))

(deftest infer-state
  (is (= (clean? (create :foo #() true :bar #() [:foo]) :bar) true))
  (is (= (clean? (create :foo #() false :bar #() [:foo]) :bar) false)))

(deftest basic-setall
  (is (= (clean? (set-all trimix true) :bar) true))
  (is (= (clean? (set-all trimix false) :baz)) false))

(def sample
  (create :dim #() true
	  :pose #() true
	  :xform #() [:pose :dim]
	  :hover nil true
	  :udata nil true
	  :painting #() [:udata :xform :hover]
	  :mode #() [:udata]
	  :toolstate #() [:mode]))

(deftest read-names
  (is (= (node-names sample)
	 #{:dim :pose :xform :hover :udata :painting :mode :toolstate})))

(deftest read-states
  (is (= (states sample)
	 {:dim true :pose true :xform true :hover true :udata true
	  :painting true :mode true :toolstate true})))

(deftest get-cleaner
  (let [updater #()
	basic (create :foo updater false :bar nil true)]
    (is (= (cleaner basic :foo) updater))
    (is (nil? (cleaner basic :bar)))))

(deftest find-dependencies
  (is (= (dependencies-1 sample :udata) #{}))
  (is (= (dependencies-1 sample :painting) #{:udata :hover :xform}))
  (is (= (dependencies   sample :painting) #{:udata :hover :xform :pose :dim})))

(deftest find-dependants
  (is (= (dependants-1 sample :udata) #{:painting :mode}))
  (is (= (dependants   sample :udata) #{:painting :mode :toolstate}))
  (is (empty? (dependants sample :toolstate))))

(deftest dirtying
  (let [d-toolstate (dirty sample :toolstate)
	d-udata (dirty sample :udata)]
    (is (= (clean? d-toolstate :toolstate) false))
    (is (= (clean? d-toolstate :dim) true))
    (is (= (clean? d-toolstate :mode) true))
    (is (= (clean? d-udata :udata) false))
    (is (= (clean? d-udata :painting) false))
    (is (= (clean? d-udata :mode) false))
    (is (= (clean? d-udata :toolstate) false))
    (is (= (clean? d-udata :xform) true))))

(def l0 (ref 0))
(def l1 (ref 0))
(def l3 (ref 0))
(def l0! (fn [] (dosync (ref-set l0 (inc @l0)))))
(def l1! (fn [] (dosync (ref-set l1 (inc @l1)))))
(def l3! (fn [] (dosync (ref-set l3 (inc @l3)))))
(def diamond (dirty (create :l0 l0! true
			    :l1 l1! [:l0]
			    :l2a nil [:l1]
			    :l2b #() [:l1]
			    :l3 l3! [:l2a :l2b])
		    :l1))

(deftest cleaning-list
  (let [{dfns :fns dnodes :nodes} (to-clean diamond :l3)]
    (is (= (count dfns) 3))
    (is (every? (complement nil?) dfns))
    (is (not (some #(= % l0!) dfns)))
    (is (some #(= % l1!) dfns))
    (is (some #(= % l3!) dfns))
    (is (distinct? dfns))
    (is (= (count dnodes) 4))
    (is (not (contains? dnodes :l0)))))

(deftest total-cleanup
  (dosync
   (ref-set l0 0)
   (ref-set l1 0)
   (ref-set l3 0)
   (let [result (clean diamond :l3)]
     (is (= @l0 0))
     (is (= @l1 1)) ; For performance. (Cleaners should be repeatable.)
     (is (= @l3 1))
     (is (clean? result :l3))
     (is (clean? result :l1)))))

