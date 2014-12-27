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

(defn colliding?
  [this that]
  (let [[this-x1 this-x2] (around (:x this) (:w this))
        [this-y1 this-y2] (around (:y this) (:h this))
        [this-z1 this-z2] (around (:z this) (:l this))
        [that-x1 that-x2] (around (:x that) (:w that))
        [that-y1 that-y2] (around (:y that) (:h that))
        [that-z1 that-z2] (around (:z that) (:l that))]
    (or (and (or (<= that-x1 this-x1 that-x2)
                 (<= that-x1 this-x2 that-x2))
             (or (<= that-y1 this-y1 that-y2)
                 (<= that-y1 this-y2 that-y2))
             (or (<= that-z1 this-z1 that-z2)
                 (<= that-z1 this-z2 that-z2)))
        (and (or (<= this-x1 that-x1 this-x2)
                 (<= this-x1 that-x2 this-x2))
             (or (<= this-y1 that-y1 this-y2)
                 (<= this-y1 that-y2 this-y2))
             (or (<= this-z1 that-z1 this-z2)
                 (<= this-z1 that-z2 this-z2))))))

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
  (assoc (box 2 2 2 (color :blue) 0 0 0 :player)
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
            x-vel (/ x-vel 20)
            z-vel (/ z-vel 20)
            x (+ x x-vel)
            z (+ z z-vel)
            moved (assoc this :x x :z z)]
        (if (colliding-any? moved entities)
          this
          moved)))))
