(ns butcher.entity.core
  (:require [play-clj.core :refer :all]
            [play-clj.g3d :refer :all]
            [play-clj.math :refer :all]
            [butcher.entity.collision :refer :all]
            [butcher.entity.quadtree :as q]))

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
  [entities]
  (q/quad-insert entities
        (assoc (box 2 2 2 (color 0.5 0.375 0.125 1) 0 (flat-y 2) 0 :player)
          :on-render
          (fn [{:keys [x y z] :as this}
              {:keys [delta-time total-time]}
              entities]
            (let [left? (key-pressed? :s)
                  right? (key-pressed? :f)
                  up? (key-pressed? :e)
                  down? (key-pressed? :d)
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
                  moved (assoc this :x x :z z)
                  nearby (q/quad-search entities [x z] 10)]
              (if (colliding-any? moved nearby)
                this
                moved))))))

(defn npcs
  [{:keys [radius] :as entities} n]
  (let [size 2
        colors (range 0.125 0.875 0.125)
        positions (range (- radius) radius)
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
                   moved (assoc this :x x :z z :x-vel x-vel :z-vel z-vel)
                   nearby (q/quad-search entities [x z] 10)]
               (if (colliding-any? moved nearby)
                 this
                 moved)))]
    (loop [n n
           entities entities]
      (let [x (rand-nth positions)
            z (rand-nth positions)
            npc (assoc (box size size size
                            (color (rand-nth colors)
                                   (rand-nth colors)
                                   (rand-nth colors)
                                   1)
                            x (flat-y size) z)
                  :on-render ai
                  :last-action-time 0
                  :x-vel 0
                  :z-vel 0)
            new-entities (q/quad-insert entities npc)
            nearby (q/quad-search entities [x z] 10)]
        (cond
         ;; if created on top of another entity, try again
         (colliding-any? npc nearby) (recur n entities)
         ;; if n <= 1, stop recursion
         (<= n 1) new-entities
         ;; else add npc to entities and continue
         :else (recur (dec n) new-entities))))))

(defn obstacles
  [{:keys [radius] :as entities} n]
  (let [sizes (range 10 30)
        color-step 0.0625
        colors (partition 3 (range color-step (- 1 color-step) color-step))
        positions (concat (range (- radius) -15) (range 15 radius))]
    (loop [n n
           entities entities]
      (let [w (rand-nth sizes)
            h (rand-nth sizes)
            l (rand-nth sizes)
            ;; color: pick random values near each other
            ;; to create more dull colored boxes
            color-group (rand-nth colors)
            r (rand-nth color-group)
            g (rand-nth color-group)
            b (rand-nth color-group)
            x (rand-nth positions)
            y (flat-y h)
            z (rand-nth positions)
            obstacle (box w h l (color r g b 1) x y z)
            new-entities (q/quad-insert entities
                               obstacle
                               ;; until i can figure out how to insert flat
                               ;; textures, place thin box underneath
                               ;; for 'shadow' to help with visibility
                               (box w 0.1 l
                                    (color (/ r 2)
                                           (/ g 2)
                                           (/ b 2)
                                           1)
                                    x 0 z))
            nearby (q/quad-search entities [x z] 10)]
        (cond
         ;; if created on top of another entity, try again
         (colliding-any? obstacle nearby) (recur n entities)
         ;; if n <= 1, stop recursion
         (<= n 1) new-entities
         ;; else add obstacle to entities and continue
         :else (recur (dec n) new-entities))))))
