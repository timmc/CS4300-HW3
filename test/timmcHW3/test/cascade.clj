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
	  :hover #() false
	  :udata #() true
	  :painting #() [:udata :xform :hover]
	  :mode #() [:udata]
	  :toolstate #() [:mode]))

(deftest find-deps
  (is (= (dependants-1 sample :udata) #{:painting :mode}))
  (is (= (dependants   sample :udata) #{:painting :mode :toolstate}))
  (is (empty? (dependants sample :toolstate))))

