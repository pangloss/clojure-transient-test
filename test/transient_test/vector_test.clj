(ns transient-test.vector-test
  (:require [clojure.test :refer :all]
            [transient-test.core :refer :all]
            [clojure.test.check.clojure-test :refer (defspec)]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn transient?
  [x]
  (instance? clojure.lang.ITransientCollection x))

(def gen-conj
  (gen/fmap (fn [x]
              [:conj x])
            gen/int))

(def gen-action
  (gen/one-of [gen-conj
               (gen/return [:pop])
               (gen/return [:transient])
               (gen/return [:persistent!])]))

(defn reduce-actions
  [coll actions]
  (reduce
    (fn [c [f & [arg]]]
      (condp = [(transient? c) f]
        [true   :conj]          (conj! c arg)
        [false  :conj]          (conj c arg)
        [true   :pop]          (if (get c 0)
                                 (pop! c)
                                 c)
        [false  :pop]          (if (seq c) (pop c) c)
        [true   :transient]     c
        [false  :transient]     (transient c)
        [true   :persistent!]   (persistent! c)
        [false  :persistent!]   c))
    coll
    actions))

(defn apply-actions
  [coll actions]
  (let [applied (reduce-actions coll actions)]
    (if (transient? applied)
      (persistent! applied)
      applied)))

(defn filter-actions
  [actions]
  (filter (fn [[a & args]]
            (#{:conj :pop} a))
          actions))

(def transient-vector
  (prop/for-all
    [a (gen/vector gen-action)]
    (= (apply-actions [] a)
       (apply-actions [] (filter-actions a)))))

(defspec transient-vector-test 10000 transient-vector)
