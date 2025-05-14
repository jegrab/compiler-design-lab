(ns compiler.frontend.common.id)

(def ^:private next-id (atom 0))

(defn make-tmp []
  (swap! next-id inc)
  (keyword (str "tmp" @next-id)))

(defn make-var []
  (swap! next-id inc)
  (keyword (str "var" @next-id)))