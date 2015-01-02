(ns butcher.entity.collision)

(defn around
  [val radius]
  (let [low (- val radius)
        high (+ val radius)]
    [low high]))

;; http://stackoverflow.com/a/27672249/1404338
(defn conflicting?
  [this that dimension size]
  (let [[this-c1 this-c2] (around (dimension this) (size this))
        [that-c1 that-c2] (around (dimension that) (size that))]
    (and (<= this-c1 that-c2)
         (<= that-c1 this-c2))))

(defn colliding?
  ([this that]
     ;; entity colliding check
     ;; entities don't collide with themselves
     ;; only entities with w/h/l can collide
     ;; check w only (optimization)
     (if (or (= (:id this) (:id that))
             (some nil? (map :w [this that])))
       false
       (colliding? this that [:x :y :z]
                   ;; w/h/l are diameters, not radii,
                   ;; so half them before calculations.
                   ;; leave them as functions!
                   (map (fn [s] #(/ (s %) 2)) [:w :h :l]))))
  ([this that dimension size]
     ;; arbitrary colliding check
     (every? true? (map #(conflicting? this that %1 %2)
                        dimension size))))

(defn colliding-any?
  [entity entities]
  (some #(colliding? entity %) entities))
