(ns butcher.entity.core
  (:require [play-clj.core :refer :all]
            [play-clj.g3d :refer :all]
            [play-clj.math :refer :all]))

;; mutating entity fns

(defn rotate!
  [entity x z]
  (let [trans (-> entity :object (. transform))
        x? (not (zero? x))
        z? (not (zero? z))
        degrees (cond
                 (and x? z?) (+ x z)
                 x? x
                 z? z
                 :else 0)]
    ;; looked at the docs, rotate is: x y z axes and degrees
    ;; set axes to rotate around (0 -> 1?) and then strength as degrees
    (matrix-4! trans :rotate (if x? 1 0) 0 (if z? 1 0) degrees)))

;; non-mutating entity fns

(defn move-flat
  [entity x z]
  (assoc entity
    :x (- (:x entity) x)
    :z (- (:z entity) z)))

(defn around
  [val radius]
  (let [half (/ radius 2)
        low (- val half)
        high (+ val half)]
    [low high]))

;; http://stackoverflow.com/a/27672249/1404338
(defn conflicting?
  [this that dimension size]
  (let [[this-c1 this-c2] (around (dimension this) (size this))
        [that-c1 that-c2] (around (dimension that) (size that))]
    (and (<= this-c1 that-c2)
         (<= that-c1 this-c2))))

(defn colliding?
  [this that]
  (every? true? (map #(conflicting? this that %1 %2)
                     [:x :y :z]
                     [:w :h :l])))

(defn colliding-any?
  [{:keys [id] :as entity} entities]
  (some
   #(colliding? entity %)
   ;; check entities other than current entity
   (filter #(not= (:id %) id) entities)))

;; entity constructors

(defn box
  "w: width
h: height
l: length
c: color
x: x position
y: y position
z: z position
id: id"
  ([w h l c x y z id]
     (let [attr (attribute! :color :create-diffuse c)
           model-mat (material :set attr)
           model-attrs (bit-or (usage :position) (usage :normal))
           builder (model-builder)]
       (-> (model-builder! builder :create-box w h l model-mat model-attrs)
           model
           (assoc :w w :h h :l l :x x :y y :z z :id id))))
  ([w h l c x y z]
     (box w h l c x y z (gensym))))

(defn player
  []
  (assoc (box 2 2 2 (color 0 0 1 1) 0 0 0 :player)
    :on-render
    (fn [{:keys [x z] :as this} entities]
      (let [left? (key-pressed? :dpad-left)
            right? (key-pressed? :dpad-right)
            up? (key-pressed? :dpad-up)
            down? (key-pressed? :dpad-down)
            x-vel (cond
                   left? -1
                   right? 1
                   :else 0)
            z-vel (cond
                   up? -1
                   down? 1
                   :else 0)
            x-vel (/ x-vel 10)
            z-vel (/ z-vel 10)
            x (+ x x-vel)
            z (+ z z-vel)
            moved (assoc this :x x :z z)]
        (if (colliding-any? moved entities)
          this
          moved)))))

(defn make-boxes
  [n]
  (let [sizes (range 3 7)
        colors (range 0 1 0.125)
        positions (range -30 30)]
    (take n (repeatedly #(box (rand-nth sizes)
                              (rand-nth sizes)
                              (rand-nth sizes)
                              (color (rand-nth colors)
                                     (rand-nth colors)
                                     (rand-nth colors)
                                     1)
                              (rand-nth positions)
                              0 ;; keep them on the flat plane
                              (rand-nth positions))))))
