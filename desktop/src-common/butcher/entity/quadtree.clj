(ns butcher.entity.quadtree)

;; quadtree
;; {:at [x z] :radius r :max-depth d :depth d ... }
;; :ne, :se, :nw, :sw -> quad children
;; children: subtree or [vals]
;; vals stored into [] when their position is the same
;; or when max depth is reached

(def default-max-depth 8)

(defn quadtree
  "create quadtree with position at [x z] and radius around at"
  ([at radius]
     (quadtree at radius default-max-depth 1))
  ([at radius max-depth]
     (quadtree at radius max-depth 1))
  ([at radius max-depth depth]
     {:at at :radius radius :max-depth max-depth :depth depth}))

(def quad-shift {:ne [+ +] :se [+ -] :nw [- +] :sw [- -]})

(defn subtree-at
  [at radius direction]
  (let [half-rad (/ radius 2)]
    (map #(%1 %2 %3) (quad-shift direction) at [half-rad half-rad])))

(defn quad-insert
  "insert value with position into quadtree"
  [{:keys [at radius max-depth depth] :as quad} {:keys [x z] :as val}]
  (let [[ox oz] at
        direction (if (< ox x)
                    (if (< oz z) :ne :se)
                    (if (< oz z) :nw :sw))
        child? (contains? quad direction)
        child (get quad direction nil)
        children? (vector? child)
        overlap? (and children?
                      (-> child first :x (= x))
                      (-> child first :z (= z)))
        subtree? (and child? (contains? child :at))
        max-depth-reached? (= depth max-depth)]
    (assoc quad direction
           (cond
            (or overlap? (and children? max-depth-reached?)) (conj child val)
            max-depth-reached? [val]
            subtree? (quad-insert child val)
            children? (loop [quad (quadtree (subtree-at at radius direction)
                                            (/ radius 2)
                                            max-depth
                                            (inc depth))
                             vals (conj child val)]
                        (if vals
                          (recur (quad-insert quad (first vals))
                                 (next vals))
                          quad))
            :else [val]))))

(defn quad-all
  "retrieve all values in quadtree"
  [{:keys [at ne se nw sw] :as quad}]
  (if at ;; if tree
    (mapcat quad-all (filter (comp not nil?) [ne se nw sw]))
    quad))
