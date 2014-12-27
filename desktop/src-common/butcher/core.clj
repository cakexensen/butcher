(ns butcher.core
  (:require [play-clj.core :refer :all]
            [play-clj.g3d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.ui :refer :all]
            [butcher.entity.core :as e]))

(def manager (asset-manager))
(set-asset-manager! manager)

(def camera-angle-z 5)

(defn update-screen!
  [screen entities]
  ;; move camera with to center on the box
  (doseq [{:keys [x z id]} entities]
    (when (= id :player)
      (position! screen x 5 (+ z camera-angle-z))))
  entities)

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen
             :renderer (model-batch)
             :camera (doto (perspective 75 (game :width) (game :height))
                       (position! 0 8 camera-angle-z)
                       (direction! 0 2 0)
                       (near! 0.1)
                       (far! 300)))
    (conj (e/make-boxes 20) (e/player)))

  :on-render
  (fn [screen entities]
    (clear! 0 0 0 1)
    (->> (for [entity entities]
           ;; call entity render fns
           (if (:on-render entity)
             ((:on-render entity) entity entities)
             entity))
         (render! screen)
         (update-screen! screen)))
  
  :on-key-down
  (fn [screen entities]
    nil)

  :on-key-up
  (fn [screen entities]
    nil)

  :on-touch-down
  (fn [screen entities]
    nil)

  :on-touch-up
  (fn [screen entities]
    nil)
  
  :on-resize
  (fn [screen entities]
    (height! screen 600)))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    (assoc (label "-" (color :white))
      :id :fps))

  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :fps (doto entity
                    (label! :set-text
                            (str (game :fps))))
             entity))
         (render! screen)))

  :on-touch-down
  (fn [screen entities]
    nil)

  :on-touch-up
  (fn [screen entities]
    nil)
  
  :on-resize
  (fn [screen entities]
    (height! screen 300)))

(defgame butcher
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))
