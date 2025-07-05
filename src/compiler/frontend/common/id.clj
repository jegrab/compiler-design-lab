(ns compiler.frontend.common.id)

(def ^:private next-id (atom 0))

(defn make-tmp []
  (swap! next-id inc)
  (keyword (str "tmp" @next-id)))

(defn make-var []
  (swap! next-id inc)
  (keyword (str "var" @next-id)))

(defn make-label
  ([] (make-label ""))
  ([name]
   (swap! next-id inc)
   (str "label_" name "_" @next-id)))

(defn make-id-num []
  (swap! next-id inc)
  @next-id)