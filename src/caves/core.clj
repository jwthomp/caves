(ns caves.core
  (:use [caves.world :only [random-world smooth-world]]
        [caves.ui.input :only [get-input process-input]]
        [caves.ui.core :only [->UI]]
        [caves.ui.drawing :only [draw-game]])
  (:require [lanterna.screen :as s]))

(def screen-size [80 24])

(defrecord Game [world uis input])

(defn run-game [game screen]
  (loop [{:keys [input uis] :as game} game]
    (when-not (empty? uis)
      (draw-game game screen)
      (if (nil? input)
        (recur (get-input game screen))
        (recur (process-input (dissoc game :input) input))))))

(defn new-game []
  (assoc (->Game nil [(->UI :start)] nil) :location [40 20]))

(defn main 
  ([screen-type] (main screen-type false))
  ([screen-type block?]
   (letfn [(go []
             (let [screen (s/get-screen screen-type)]
               (s/in-screen screen
                            (run-game (new-game) screen))))]
      (if block?
        (go)
        (future (go))))))


(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text")  :text
                      :else           :auto)]
    (main screen-type true)))
