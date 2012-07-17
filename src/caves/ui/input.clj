(ns caves.ui.input
  (:use [caves.world :only [random-world smooth-world]]
        [caves.ui.core :only [->UI]])
  (:require [lanterna.screen :as s]))

(def screen-size [80 24])

(defmulti process-input
  (fn [game input]
    (:kind (last (:uis game)))))

(defn move [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defmethod process-input :play [game input]
  (case input
    :enter      (assoc game :uis [(->UI :win)])
    :backspace  (assoc game :uis [(->UI :lose)])
    \s          (update-in game [:world] smooth-world)  
    \q          (assoc game :uis [])
    \j          (update-in game [:location] move [-1 0])
    \k          (update-in game [:location] move [0 1])
    \i          (update-in game [:location] move [0 -1])
    \l          (update-in game [:location] move [1 0])
    game))

(defmethod process-input :start [game input]
  (-> game
    (assoc :world (random-world))
    (assoc :uis [(->UI :play)])))

(defmethod process-input :win [game input]
  (if (= input :escape)
      (assoc game :uis [])
      (assoc game :uis [(->UI :start)])))

(defmethod process-input :lose [game input]
  (if (= input :escape)
      (assoc game :uis [])
      (assoc game :uis [(->UI :start)])))

(defn get-input [game screen]
  (assoc game :input (s/get-key-blocking screen)))

