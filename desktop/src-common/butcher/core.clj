(ns butcher.core
  (:require [play-clj.core :refer :all]
            [play-clj.g3d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.ui :refer :all]))

(def manager (asset-manager))
(set-asset-manager! manager)

(defn rotate
  [entity x y]
  (let [trans (-> entity :object (. transform))]
    (matrix-4! trans :rotate 0 x y 1)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen
             :renderer (model-batch)
             :camera (doto (perspective 75 (game :width) (game :height))
                       (position! 0 5 5)
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
  (fn [screen entities]
    (clear! 0 0 0 1)
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
               :box (if dragging
                      (doto entity
                        (rotate drag-x drag-y))
                      entity)
               entity))
           (render! screen))))
  
  :on-key-down
  (fn [screen entities])

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
    (height! screen 300)))

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
    (height! screen 300)))

(defgame butcher
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))
