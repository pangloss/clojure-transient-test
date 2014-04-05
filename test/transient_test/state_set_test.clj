(ns transient-test.state-set-test
  (:use transient-test.state)
  (:require [clojure.test :refer :all]
            [transient-test.core :refer :all]
            [clojure.test.check.clojure-test :refer (defspec)]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn new-state [value]
  {:contents {}
   :transient false})

(defmethod next-state :conj [_ s value arg]
  (assoc-in s [:contents arg] true))

(defmethod next-value :conj [_ s value arg]
  (if (:transient s)
    (conj! value arg)
    (conj value arg)))

(defn incorrect? [s result]
  (when-not (:transient s)
    (not= (into #{} (keys (:contents s))) result)))

(defmethod error? :conj [_ s result arg]
  (cond (not (result arg)) "Argument should be present in the result"
        (incorrect? s result) "Expected state does not match result"))

(defmethod next-state :disj [_ s value arg]
  (update-in s [:contents] dissoc arg))

(defmethod next-value :disj [_ s value arg]
  (if (:transient s)
    (disj! value arg)
    (disj value arg)))

(defmethod error? :disj [_ s result arg]
  (cond (incorrect? s result) "Expected state does not match result"))

(defmethod pre :transient [_ s]
  (not (:transient s)))

(defmethod next-state :transient [_ s _ _]
  (assoc s :transient true))

(defmethod next-value :transient [_ s value _]
  (transient value))

(defn transient?
  [x]
  (instance? clojure.lang.ITransientCollection x))

(defmethod error? :transient [_ s result _]
  (cond (not (transient? result)) "Result should be transient"))

(defmethod pre :persistent! [_ s]
  (:transient s))

(defmethod next-state :persistent! [_ s _ _]
  (assoc s :transient false))

(defmethod next-value :persistent! [_ _ value _]
  (persistent! value))

(defmethod error? :persistent! [_ _ result _]
  (cond (transient? result) "Result should not be transient"))

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
               gen-disj
               (gen/return [:transient])
               (gen/return [:persistent!])]))

(defn apply-actions [s coll actions]
  (reduce-actions s coll (conj (vec actions) [:persistent!])))

(defn filter-actions
  [actions]
  (filter (fn [[a & args]]
            (#{:conj :disj} a))
          actions))

(def transient-state-set
  (prop/for-all
    [a (gen/vector gen-action)]
    (let [init-value #{}
          s (new-state init-value)
          [state1 result1] (apply-actions s init-value a)
          [state2 result2] (apply-actions s init-value (filter-actions a))]
      (cond (not= result1 result2) (throw (ex-info "Result mismatch" {1 result1 2 result2}))
            (not= state1 state2) (throw (ex-info "State mismatch" {1 state1 2 state2}))
            (not= (count (:contents state1)) (count result1)) (throw (ex-info "State wrong" {:state state1 :result result1}))
            :else true))))

(defspec transient-state-set-test 10000 transient-state-set)


