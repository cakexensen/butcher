(ns butcher.core
  (:require [play-clj.core :refer :all]
            [play-clj.g3d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.ui :refer :all]
            [butcher.entity.core :as e])
  (:import [com.badlogic.gdx.graphics Texture$TextureFilter]
           [com.badlogic.gdx.graphics.g2d SpriteBatch]
           [com.badlogic.gdx.graphics.glutils FrameBuffer]))

(def manager (asset-manager))
(set-asset-manager! manager)

(def screen-width 400)
(def screen-height 240)

(def default-screen-width (* 3 screen-width))
(def default-screen-height (* 3 screen-height))

(def resolution-step 0.125)

(def camera-y 3)
(def camera-z 8)

(defn update-screen!
  [screen entities]
  ;; move camera with to center on the box
  (doseq [{:keys [x z id]} entities]
    (when (= id :player)
      (position! screen x camera-y (+ z camera-z))))
  entities)

(defn setup-pixelate!
  "change pixelation factor for rendering"
  ([screen]
     (setup-pixelate! screen 1))
  ([{:keys [fbo fbo-batch] :as screen} resolution-modifier]
     (when fbo
       (.dispose fbo))
     (when fbo-batch
       (.dispose fbo-batch))
     (let [screen-height (* resolution-modifier screen-height)]
       (update! screen
                :fbo (doto (FrameBuffer. (pixmap-format :r-g-b-888)
                                         ;; compute an appropriate width
                                         ;; based on real screen resolution
                                         (/ (game :width)
                                            (/ (game :height) screen-height))
                                         screen-height
                                         true)
                       (-> .getColorBufferTexture
                           (.setFilter
                            Texture$TextureFilter/Nearest
                            Texture$TextureFilter/Nearest)))
                :fbo-batch (SpriteBatch.)
                :resolution-modifier resolution-modifier))))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen
             :renderer (model-batch)
             :camera (doto (perspective 75 (game :width) (game :height))
                       (position! 0 camera-y camera-z)
                       (direction! 0 (dec camera-y) 0)
                       (near! 0.1)
                       (far! 300)))
    (setup-pixelate! screen)
    (-> [] (e/obstacles 50) (e/npcs 10) (e/player)))

  :on-render
  (fn [{:keys [fbo fbo-batch] :as screen} entities]
    ;; fbo/batch: for pixelation; separate later
    (.begin fbo)
    (clear! 0 0 0 1)
    (let [entities
          (->> (flatten
                (for [entity entities]
                  ;; call entity render fns
                  (if (:on-render entity)
                    ((:on-render entity) entity screen entities)
                    entity)))
               (render! screen)
               (update-screen! screen))]
      (.end fbo)
      (.begin fbo-batch)
      (.draw fbo-batch
             (.getColorBufferTexture fbo)
             0 0 (game :width) (game :height) 0 0 1 1)
      (.end fbo-batch)
      entities))
  
  :on-key-down
  (fn [{:keys [key resolution-modifier] :as screen} entities]
    (cond
     (= key (key-code :page-down))
     (do (setup-pixelate! screen (max (- resolution-modifier
                                         resolution-step)
                                      resolution-step)))
     (= key (key-code :page-up))
     (do (setup-pixelate! screen (min (+ resolution-modifier
                                         resolution-step)
                                      2))))
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
    (height! screen default-screen-height)
    (setup-pixelate! screen)
    nil))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    (assoc (label "-" (color :white))
      :id :fps :x 5))

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
    (height! screen screen-height)))

(defgame butcher
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))
