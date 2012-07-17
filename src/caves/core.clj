(ns caves.core
  (:use [caves.world :only [random-world smooth-world]])
  (:require [lanterna.screen :as s]))

(def screen-size [80 24])

(defrecord UI [kind])
(defrecord Game [world uis input])

(defn clear-screen [screen]
  (let [[cols rows] screen-size
        blank (apply str (repeat cols \space))]
    (doseq [row (range rows)]
      (s/put-string screen 0 row blank))))

(defmulti draw-ui
  (fn [ui game screen]
    (:kind ui)))

(defmethod draw-ui :start [ui game screen]
  (s/put-string screen 0 0 "Welcome to the Caves of Clojure!"))

(defmethod draw-ui :win [ui game screen]
  (s/put-string screen 0 0 "You win."))

(defmethod draw-ui :lose [ui game screen]
  (s/put-string screen 0 0 "You lose."))

(defmethod draw-ui :play [ui {{:keys [tiles]} :world :as game} screen]
  (let [[cols rows] screen-size
        vcols cols
        vrows (dec rows)
        start-x 0
        start-y 0
        end-x (+ start-x vcols)
        end-y (+ start-y vrows)]
    (doseq [[vrow-idx mrow-idx] (map vector
                                      (range 0 vrows)
                                      (range start-y end-y))
            :let [row-tiles (subvec (tiles mrow-idx) start-x end-x)]]
      (doseq [vcol-idx (range vcols)
              :let [{:keys [glyph color]} (row-tiles vcol-idx)]]
        (s/put-string screen vcol-idx vrow-idx glyph {:fg color})))))


(defn draw-game [game screen]
  (clear-screen screen)
  (doseq [ui (:uis game)]
    (draw-ui  ui game screen))
  (s/redraw screen))

(defmulti process-input
  (fn [game input]
    (:kind (last (:uis game)))))

(defmethod process-input :play [game input]
  (case input
    :enter      (assoc game :uis [(new UI :win)])
    :backspace  (assoc game :uis [(new UI :lose)])
    \s          (assoc game :world (smooth-world (:world game)))
    game))

(defmethod process-input :start [game input]
  (-> game
    (assoc :world (random-world))
    (assoc :uis [(new UI :play)])))

(defn get-input [game screen]
  (assoc game :input (s/get-key-blocking screen)))

(defn run-game [game screen]
  (loop [{:keys [input uis] :as game} game]
    (when-not (empty? uis)
      (draw-game game screen)
      (if (nil? input)
        (recur (get-input game screen))
        (recur (process-input (dissoc game :input) input))))))

(defn new-game []
  (new Game nil [(new UI :start)] nil))

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
