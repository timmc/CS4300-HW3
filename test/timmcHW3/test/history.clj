(ns timmcHW3.test.history
  (:require [timmcHW3.history :as h] :reload)
  (:use [clojure.test]))

(deftest blank
  (let [cur (Object.)
	basic (h/create cur)]
    (is (identical? (h/current basic) cur))
    (is (not (h/undo? basic)))
    (is (not (h/redo? basic)))))

(def end5
  (-> (h/create 0) (h/act 1) (h/act 2) (h/act 3) (h/act 4)))
(def mid5
  (-> end5 (h/undo) (h/undo)))
(def begin5
  (-> mid5 (h/undo) (h/undo)))
(def redone5
  (-> begin5 (h/undo) (h/undo) (h/undo) (h/undo)))

(deftest actions
  (is (= (h/current end5) 4))
  (is (h/undo? end5))
  (is (not (h/redo? end5)))
  (is (= (h/peek-past end5) 3))
  (is (nil? (h/peek-future end5))))

(deftest partial-undo
  (is (= (h/current mid5) 2))
  (is (h/undo? mid5))
  (is (h/redo? mid5))
  (is (= (h/peek-past mid5) 1))
  (is (= (h/peek-future mid5) 3)))

(deftest drop-redo
  (let [fork (h/act mid5 "a")]
    (is (= (h/current fork) "a"))
    (is (not (h/redo? fork)))
    (is (= (h/peek-past fork) 2))))

(deftest ignore-bad-movement
  (is (identical? (h/redo end5) end5))
  (is (identical? (h/undo begin5) begin5)))

