(ns butcher.entity.quadtree
  (:require [butcher.entity.collision :refer :all]))

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

(defn shift-at-radius
  [shift at radius]
  (map #(%1 %2 radius) shift at))

(defn subtree-at
  [at radius direction]
  (shift-at-radius (quad-shift direction) at (/ radius 2)))

(defn quad-insert
  "insert value with position into quadtree"
  ([quad val & vals]
     (loop [quad (quad-insert quad val)
            vals vals]
       (if vals
         (recur (quad-insert quad (first vals)) (next vals))
         quad)))
  ([{:keys [at radius max-depth depth] :as quad} {:keys [x z] :as val}]
     (let [[ox oz] at
           direction (if (< ox x)
                       (if (< oz z) :ne :se)
                       (if (< oz z) :nw :sw))
           child (get quad direction nil)
           children? (vector? child)
           overlap? (and children?
                         (-> child first :x (= x))
                         (-> child first :z (= z)))
           subtree? (and child (contains? child :at))
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
               :else [val])))))

(defn quad-all
  "retrieve all values in quadtree"
  [{:keys [at ne se nw sw] :as quad}]
  (if at ;; if tree
    (mapcat quad-all (filter (comp not nil?) [ne se nw sw]))
    quad))

(defn quad-intersects?
  "checks if quad-style boxes intersect"
  [this-at this-radius that-at that-radius]
  ;; sloppily reuse existing collision logic
  ;; maybe refactor these better?
  (let [[x1 z1] this-at
        [x2 z2] that-at
        this {:x x1 :z z1 :r this-radius}
        that {:x x2 :z z2 :r that-radius}]
    (colliding? this that [:x :z] [:r :r])))

(defn quad-search
  "search quadtree based on bounding box"
  [{:keys [at radius ne se nw sw] :as quad} search-at search-radius]
  (if (quad-intersects? at radius search-at search-radius)
    (let [directions [ne se nw sw]
          children (flatten (filter vector? directions))
          subtrees (filter map? directions)]
      (concat (filter #(quad-intersects? (map get [% %] [:x :z])
                                         (or (:w % nil) (/ radius 2))
                                         search-at search-radius)
                      children)
              (mapcat #(quad-search % search-at search-radius) subtrees)))
    []))
