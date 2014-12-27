(ns butcher.core
  (:require [play-clj.core :refer :all]
            [play-clj.g3d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.ui :refer :all]))

(def manager (asset-manager))
(set-asset-manager! manager)

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
    ;; looked at the docs, rotate ix x y z axes and degrees
    ;; set axes to rotate around (0 -> 1?) and then strength as degrees
    (matrix-4! trans :rotate (if x? 1 0) 0 (if z? 1 0) degrees)))

(defn move-flat
  [entity x z]
  (assoc entity
    :x (- (:x entity) x)
    :z (- (:z entity) z)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen
             :renderer (model-batch)
             :camera (doto (perspective 75 (game :width) (game :height))
                       (position! 3 3 -3)
                       (direction! 0 0 0)
                       (near! 0.1)
                       (far! 300)))
    (let [attr (attribute! :color :create-diffuse (color :blue))
          model-mat (material :set attr)
          model-attrs (bit-or (usage :position) (usage :normal))
          builder (model-builder)]
      (-> (model-builder! builder :create-box 2 2 2 model-mat model-attrs)
          model
          (assoc :x 0 :y 0 :z 0 :id :box))))

  :on-render
  (fn [{:keys [drag-x drag-y move-x move-z] :as screen} entities]
    (clear! 0 0 0 1)
    (let [x (game :x)
          y (game :y)
          delta-x (- x (or drag-x x))
          delta-y (- y (or drag-y y))
          move-x (/ (or move-x 0) 20)
          move-z (/ (or move-z 0) 20)]
      (when drag-x
        (update! screen
                 :drag-x x
                 :drag-y y))
      (->> (for [entity entities]
             (case (:id entity)
               :box (move-flat (doto entity
                                  (rotate! delta-x delta-y))
                                move-x move-z)
               entity))
           (render! screen))))
  
  :on-key-down
  (fn [screen entities]
    (condp = (:key screen)
      (key-code :dpad-left) (update! screen :move-x -1)
      (key-code :dpad-right) (update! screen :move-x 1)
      (key-code :dpad-up) (update! screen :move-z 1)
      (key-code :dpad-down) (update! screen :move-z -1))
    nil)

  :on-key-up
  (fn [screen entities]
    (condp = (:key screen)
      (key-code :dpad-left) (update! screen :move-x 0)
      (key-code :dpad-right) (update! screen :move-x 0)
      (key-code :dpad-up) (update! screen :move-z 0)
      (key-code :dpad-down) (update! screen :move-z 0))
    nil)

  :on-touch-down
  (fn [screen entities]
    (update! screen
             :drag-x (game :x)
             :drag-y (game :y))
    nil)

  :on-touch-up
  (fn [screen entities]
    (update! screen
             :drag-x nil
             :drag-y nil)
    nil)
  
  :on-resize
  (fn [screen entities]
    (height! screen 600)))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    (assoc (label "-" (color :white))
      :id :drag))

  :on-render
  (fn [screen entities]
    (let [dragging (:dragging screen)
          drag-x (:drag-x screen)
          drag-y (:drag-y screen)
          x (game :x)
          y (game :y)]
      (update! screen
               :drag-x (- x (or drag-x x))
               :drag-y (- y (or drag-y y)))
      (->> (for [entity entities]
             (case (:id entity)
               :drag (doto entity
                       (label! :set-text
                               (if dragging
                                 (str "drag-x: " drag-x
                                      " drag-y: " drag-y)
                                 (str (game :fps)))))
               entity))
           (render! screen))))

  :on-touch-down
  (fn [screen entities]
    (update! screen :dragging true)
    nil)

  :on-touch-up
  (fn [screen entities]
    (update! screen :dragging false)
    nil)
  
  :on-resize
  (fn [screen entities]
    (height! screen 600)))

(defgame butcher
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))
