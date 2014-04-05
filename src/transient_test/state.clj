(ns transient-test.state)

(defn first-arg [arg & args] arg)

(defmulti next-state first-arg)
(defmulti next-value first-arg)
(defmulti pre first-arg)
(defmulti error? first-arg)

(defmethod pre :default [_ s]
  true)

(defmethod error? :default [_ s]
  nil)

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
