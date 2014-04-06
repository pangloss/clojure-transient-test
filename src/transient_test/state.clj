(ns transient-test.state)

(defmulti pre (fn [action state]))
(defmulti next-state (fn [action state result arg]))
(defmulti next-value (fn [action state result arg]))
(defmulti error? (fn [action state result arg]))

(defmethod pre :default [_ s]
  true)

(defmethod error? :default [_ s result arg]
  nil)

(defn simulate-actions [s coll actions]
  (reduce
    (fn [[s value] [action arg]]
      (if (pre action s)
        (let [s' (next-state action s value arg)
              result (next-value action s value arg)]
          (if-let [error (error? action s' result arg)]
            (throw (ex-info (str "Postcondition failed: " error)
                            {:pre-state s :post-state s' :action action :arg arg
                             :value value :result result}))
            [s' result]))
        [s value]))
    [s coll]
    actions))
