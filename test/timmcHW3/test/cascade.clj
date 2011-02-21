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
	  :hover #() true
	  :udata #() true
	  :painting #() [:udata :xform :hover]
	  :mode #() [:udata]
	  :toolstate #() [:mode]))

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

