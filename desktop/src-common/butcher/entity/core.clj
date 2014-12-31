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

(defn flat-y
  [h]
  (/ h 2))

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
  ;; entities don't collide with themselves
  ;; only entities with w/h/l can collide
  (if (or (= (:id this) (:id that))
          (some nil? (mapcat #(map % [this that]) [:w :h :l])))
    false
    (every? true? (map #(conflicting? this that %1 %2)
                       [:x :y :z]
                       [:w :h :l]))))

(defn colliding-any?
  [entity entities]
  (some #(colliding? entity %) entities))

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
  (assoc (box 2 2 2 (color 0.5 0.375 0.125 1) 0 (flat-y 2) 0 :player)
    :on-render
    (fn [{:keys [x y z] :as this}
        {:keys [delta-time total-time]}
        entities]
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

(defn npcs
  [n]
  (let [size 2
        colors (range 0.125 0.875 0.125)
        positions (range -50 50)
        ai (fn [{:keys [x z x-vel z-vel last-action-time] :as this}
               {:keys [delta-time]}
               entities]
             (let [;; reduce how often ai takes actions
                   can-act? (< (* (rand) 8) last-action-time)
                   last-action-time (if can-act?
                                      0
                                      (+ delta-time last-action-time))
                   this (assoc this :last-action-time last-action-time)
                   move? (< 0.5 (rand))
                   x-vel (if can-act?
                           (if move?
                             (/ (rand-nth [-1 0 1]) 10)
                             0)
                           x-vel)
                   z-vel (if can-act?
                           (if move?
                             (/ (rand-nth [-1 0 1]) 10)
                             0)
                           z-vel)
                   x (+ x x-vel)
                   z (+ z z-vel)
                   moved (assoc this :x x :z z :x-vel x-vel :z-vel z-vel)]
               (if (colliding-any? moved entities)
                 this
                 moved)))]
    (take n (repeatedly
             #(assoc (box size size size
                          (color (rand-nth colors)
                                 (rand-nth colors)
                                 (rand-nth colors)
                                 1)
                          (rand-nth positions)
                          (flat-y size)
                          (rand-nth positions))
                :on-render ai
                :last-action-time 0
                :x-vel 0
                :z-vel 0)))))

(defn obstacles
  [n]
  (let [sizes (range 5 10)
        colors (range 0.125 0.875 0.125)
        near-color (range -0.125 0.125 0.125)
        positions (range -60 60)]
    (take n (repeatedly
             #(let [w (rand-nth sizes)
                    h (rand-nth sizes)
                    l (rand-nth sizes)
                    ;; color: pick random values near each other
                    ;; to create more dull colored boxes
                    r (rand-nth colors)
                    g (+ r (rand-nth near-color))
                    b (+ r (rand-nth near-color))
                    x (rand-nth positions)
                    y (flat-y h)
                    z (rand-nth positions)]
                (box w h l (color r g b 1) x y z))))))
