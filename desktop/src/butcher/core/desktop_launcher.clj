(ns butcher.core.desktop-launcher
  (:require [butcher.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. butcher "butcher" default-screen-width default-screen-height)
  (Keyboard/enableRepeatEvents true))
