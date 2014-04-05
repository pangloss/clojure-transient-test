(ns transient-test.state2-test
  (:require [clojure.test :refer :all]
            [transient-test.core :refer :all]
            [clojure.test.check.clojure-test :refer (defspec)]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn new-state [value]
  {:contents (into [] value)
   :transient false})

(defn first-arg [arg & args] arg)

(defmulti next-state first-arg)
(defmulti next-value first-arg)
(defmulti pre first-arg)
(defmulti error? first-arg)

(defmethod pre :conj [_ s]
  true)

(defmethod next-state :conj [_ s value arg]
  (update-in s [:contents] conj arg))

(defmethod next-value :conj [_ s value arg]
  (if (:transient s)
    (conj! value arg)
    (conj value arg)))

(defn contents [s result]
  (if (:transient s)
    (:contents s)
    result))

(defmethod error? :conj [_ s result arg]
  (cond (not= arg (last (:contents s))) "Argument should be present in the result"
        (not= (:contents s) (contents s result)) "Expected state does not match result"))

(defmethod pre :pop [_ s]
  (seq (:contents s)))

(defmethod next-state :pop [_ s value arg]
  (update-in s [:contents] pop))

(defmethod next-value :pop [_ s value arg]
  (if (:transient s)
    (pop! value)
    (pop value)))

(defmethod error? :pop [_ s result arg]
  (cond (not= (:contents s) (contents s result)) "Expected sState does not match result"))

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

(def gen-action
  (gen/one-of [gen-conj
               (gen/return [:pop])
               (gen/return [:transient])
               (gen/return [:persistent!])]))

(defn reduce-actions [s coll actions]
  (reduce
    (fn [[s value] [action arg]]
      (if (pre action s)
        (let [s' (next-state action s value arg)
              result (next-value action s value arg)]
          (if-let [error (error? action s' result arg)]
            (throw (ex-info (str "Postcondition failed: " error) {:pre-state s :post-state s' :action action :arg arg :value value :result result}))
            [s' result]))
        [s value]))
    [s coll]
    actions))

(defn apply-actions [s coll actions]
  (reduce-actions s coll (conj (vec actions) [:persistent!])))

(defn filter-actions
  [actions]
  (filter (fn [[a & args]]
            (#{:conj :pop} a))
          actions))

(def transient-state2
  (prop/for-all
    [a (gen/vector gen-action)]
    (let [init-value []
          s (new-state init-value)
          [state1 result1] (apply-actions s init-value a)
          [state2 result2] (apply-actions s init-value (filter-actions a))]
      (cond (not= result1 result2) (throw (ex-info "Result mismatch" {1 result1 2 result2}))
            (not= state1 state2) (throw (ex-info "State mismatch" {1 state1 2 state2}))
            (not= (count (:contents state1)) (count result1)) (throw (ex-info "State wrong" {:state state1 :result result1}))
            :else true))))

(defspec transient-state2-test 10000 transient-state2)

