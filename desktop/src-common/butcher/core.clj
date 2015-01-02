(ns butcher.core
  (:require [play-clj.core :refer :all]
            [play-clj.g3d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.ui :refer :all]
            [butcher.entity.core :as e]
            [butcher.entity.quadtree :as q])
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

(def camera-y 100)
(def camera-z 8)

(def world-size 300)

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

(defn populate!
  ([screen]
     (populate! screen 200 20))
  ([screen obstacles npcs]
     (update! screen :obs-count obstacles :npc-count npcs)
     (q/quad-all ;; use quadtree for initialization, then revert to vec
      (-> (q/quadtree [0 0] world-size (:quad-max-depth screen))
          (e/player) (e/obstacles obstacles) (e/npcs npcs)))))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    [(assoc (label "" (color :white))
       :id :fps :x 5 :y 10)
     (assoc (label "" (color :white))
       :id :depth :x 70 :y 10)
     (assoc (label "" (color :white))
       :id :obs :x 200 :y 10)
     (assoc (label "" (color :white))
       :id :npc :x 320 :y 10)])

  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :fps (doto entity
                    (label! :set-text
                            (str "fps " (game :fps))))
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
    (height! screen screen-height))

  :on-params-change
  (fn [{:keys [quad-max-depth obs-count npc-count] :as screen} entities]
    (->> (for [entity entities]
           (case (:id entity)
             :depth (doto entity
                      (label! :set-text (str "quadtree depth " quad-max-depth)))
             :npc (doto entity
                    (label! :set-text (str "npcs " npc-count)))
             :obs (doto entity
                    (label! :set-text (str "obstacles " obs-count)))
             entity)))))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen
             :renderer (model-batch)
             :camera (doto (perspective 75 (game :width) (game :height))
                       (position! 0 camera-y camera-z)
                       (direction! 0 0 0)
                       (near! 0.1)
                       (far! 300))
             :quad-max-depth 8)
    (setup-pixelate! screen)
    (populate! screen))

  :on-hide
  (fn [{:keys [fbo fbo-batch] :as screen} entities]
    (when fbo
      (.dispose fbo))
    (when fbo-batch
      (.dispose fbo-batch))
    nil)

  :on-render
  (fn [{:keys [fbo fbo-batch quad-max-depth
              obs-count npc-count] :as screen} entities]
    (screen! text-screen :on-params-change
             :quad-max-depth quad-max-depth
             :obs-count obs-count
             :npc-count npc-count)
    ;; fbo/batch: for pixelation; separate later
    (.begin fbo)
    (clear! 0 0 0 1)
    (let [quadtree (apply q/quad-insert
                          (q/quadtree [0 0] world-size quad-max-depth)
                          entities)
          entities
          (->> (flatten
                (for [entity entities]
                  ;; call entity render fns
                  (if (:on-render entity)
                    ((:on-render entity) entity screen quadtree)
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
  (fn [{:keys [key quad-max-depth obs-count npc-count] :as screen} entities]
    (cond
     (= key (key-code :page-down))
     (do (update! screen :quad-max-depth (max (dec quad-max-depth) 1)) nil)
     (= key (key-code :page-up))
     (do (update! screen :quad-max-depth (inc quad-max-depth)) nil)
     (= key (key-code :dpad-up))
     (populate! screen (+ obs-count 20) npc-count)
     (= key (key-code :dpad-down))
     (populate! screen (max (- obs-count 20) 20) npc-count)
     (= key (key-code :dpad-right))
     (populate! screen obs-count (+ npc-count 5))
     (= key (key-code :dpad-left))
     (populate! screen obs-count (max (- npc-count 5) 5))))

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

(defgame butcher
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))
