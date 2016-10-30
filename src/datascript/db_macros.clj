(ns datascript.db-macros)


(defmacro raise [& fragments]
  (let [msgs (butlast fragments)
        data (last fragments)]
    `(throw (ex-info (str ~@(map (fn [m#] (if (string? m#) m# (list 'pr-str m#))) msgs)) ~data))))

(defmacro defrecord-updatable [name fields & impls]
    `(do
       (defrecord ~name ~fields)
       (extend-type ~name ~@impls)))

;; ----------------------------------------------------------------------------
;; datom cmp macros/funcs
;;

(defmacro combine-cmp [& comps]
  (loop [comps (reverse comps)
         res   (num 0)]
    (if (not-empty comps)
      (recur
        (next comps)
        `(let [c# ~(first comps)]
           (if (== 0 c#)
             ~res
             c#)))
      res)))

(defn- -case-tree [queries variants]
  (if queries
    (let [v1 (take (/ (count variants) 2) variants)
          v2 (drop (/ (count variants) 2) variants)]
      (list 'if (first queries)
            (-case-tree (next queries) v1)
            (-case-tree (next queries) v2)))
    (first variants)))

(defmacro case-tree [qs vs]
  (-case-tree qs vs))


(defmacro cond-let [& clauses]
  (when-let [[test expr & rest] clauses]
    `(~(if (vector? test) 'if-let 'if) ~test
           ~expr
           (cond-let ~@rest))))

