(ns transient-test.state-test
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

(def gen-disj
  (gen/fmap (fn [x]
              [:disj x])
            gen/int))

(def gen-action
  (gen/one-of [gen-conj
               (gen/return [:pop])
               (gen/return [:transient])
               (gen/return [:persistent!])]))

(defn reduce-actions
  [simulated-state coll actions]
  ; in this example, simulated state is just length count
  (reduce
    (fn [[ss c] [f & [arg]]]
      [(condp = f
         :conj (inc ss)
         :pop (max 0 (dec ss))
         ss)
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
        [false  :persistent!]   c)])
    [simulated-state coll]
    actions))

(defn apply-actions
  [simulated-state coll actions]
  (let [[applied-state applied] (reduce-actions simulated-state coll actions)]
    [applied-state
     (if (transient? applied)
       (persistent! applied)
       applied)]))

(defn filter-actions
  [actions]
  (filter (fn [[a & args]]
            (#{:conj :pop} a))
          actions))

(def transient-state
  (prop/for-all
    [a (gen/vector gen-action)]
    (let [simulated-state 0
          [state1 result1] (apply-actions simulated-state [] a)
          [state2 result2] (apply-actions simulated-state [] (filter-actions a))]
      (cond (not= result1 result2) (throw (ex-info "Result mismatch" {1 result1 2 result2}))
            (not= state1 state2) (throw (ex-info "State mismatch" {1 state1 2 state2}))
            (not= state1 (count result1)) (throw (ex-info "State wrong" {:state state1 :result result1}))
            :else true))))

(defspec transient-state-test 10000 transient-state)
