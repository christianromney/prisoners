(ns prisoners.utils)

(defn add-to
  "General-purpose map collection modifier."
  [m k v]
  (update-in m [k] conj v))
