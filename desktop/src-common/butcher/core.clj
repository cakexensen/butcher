(ns butcher.core
  (:require [play-clj.core :refer :all]
            [play-clj.g3d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.ui :refer :all]
            [butcher.entity.core :as e]))

(def manager (asset-manager))
(set-asset-manager! manager)

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
    (assoc (e/box 2 2 2 (color :blue) 0 0 0 :box)
      :on-render (fn [{:keys [x z] :as this}]
                   (let [left? (key-pressed? :dpad-left)
                         right? (key-pressed? :dpad-right)
                         up? (key-pressed? :dpad-up)
                         down? (key-pressed? :dpad-down)
                         x-vel (cond
                                left? -1
                                right? 1
                                :else 0)
                         z-vel (cond
                                up? 1
                                down? -1
                                :else 0)
                         x-vel (/ x-vel 20)
                         z-vel (/ z-vel 20)
                         x (+ x x-vel)
                         z (+ z z-vel)]
                     (assoc this :x x :z z)))))

  :on-render
  (fn [screen entities]
    (clear! 0 0 0 1)
    (->> (for [entity entities]
           ;; call entity render fns
           (if (:on-render entity)
             ((:on-render entity) entity)
             entity))
         (render! screen)))
  
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
    (height! screen 600)))

(defgame butcher
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))
