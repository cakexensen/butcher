(ns butcher.core.desktop-launcher
  (:require [butcher.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. butcher "butcher" 800 600)
  (Keyboard/enableRepeatEvents true))
