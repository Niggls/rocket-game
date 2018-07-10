(ns rocket-game.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(defn star-color-index [x]
 (let [white-index 0
       yellow-index 1
       red-index 2]
  (if (< x 12)
      white-index
      (if (< x 15)
          yellow-index
          red-index))))

(defn create-star [y x]
 {:x (rand-int x)
   :y (rand-int y)
   :size (+ (rand-int 5) 1)
   :speed (+ (rand-int 3) 1)
   :color (star-color-index (rand-int 20))})

;; function for reset state
(defn reset-state-variable [state]
 (let [{:keys [score highscore]} state]
   (assoc state
           :rocket {:image (q/load-image "images/rocket.png")
                    :x 260
                    :y 340
                    :dir 0}
           :smoke []
           :score 0
           :highscore (max score highscore)
           :game-over true
           :meteors []
           :bonus {})))

;; setup: here we define our global state variable
;; # --> anonymous function
(defn setup []
  {:rocket {:image (q/load-image "images/rocket.png")
            :x 260
            :y 340
            :dir -1}
   :score 0
   :smoke []
   :highscore 0
   :stars (take 50 (repeatedly #(create-star (q/height) (q/width))))
   :game-over false
   :meteors []
   :bonus {}})

;;;; helper methods;;;;;;;;;;;;;;;;;;;;;;;;
(defn inside? [x y width height]
  (or
   (< x -12)
   (> (+ x 33) width)
   (< y 0)
   (> (+ y 40) height)))

(defn item-outside? [item height]
 (let [x (:x item)
       y (:y item)]
  (> y height)))

;; --> (fn [star height] (imte-inside? star height))
(defn remove-stars [stars height]
 (remove #(item-outside? % height) stars))


(defn meteor-out [state height]
  (let [old-count (-> state :meteors (count))
        new-meteor (remove #(item-outside? % height) (:meteors state))
        new-count (count new-meteor)]
    (-> state
         (assoc :meteors new-meteor)
         (update :score #(+ % (- old-count new-count))))))

(defn meteor-collision? [meteors rocket-x rocket-y]
  (loop [[m1 & rest] meteors]
    (if (or (and
              (<= (:x m1) rocket-x (+ (:x m1) 45))
              (<= (:y m1) rocket-y (+ (:y m1) 45)))
            (and
              (<= (:x m1) (+ rocket-x 40) (+ (:x m1) 45))
              (<= (:y m1) (+ rocket-y 40) (+ (:y m1) 45))))
      true
      (if (empty? rest)
        false
        (recur rest)))))

(defn meteor-hit [state]
  (let [rocket-x (-> state :rocket :x)
        rocket-y (-> state :rocket :y)
        meteors (:meteors state)]
    (cond
      (empty? meteors) state
      (meteor-collision? meteors rocket-x rocket-y) (reset-state-variable state)
      :else state)))

(defn bonus-out [state height]
 (if (empty? (:bonus state))
     state
     (if (item-outside? (:bonus state) height)
       (assoc state
              :bonus {})
       state)))

(defn bonus-collision? [rocket-x rocket-y bonus]
 (or (and
       (<= rocket-x  (:x bonus) (+ rocket-x 50))
       (<= rocket-y  (:y bonus) (+ rocket-y 50)))
     (and
       (<= rocket-x  (+ (:x bonus) 40) (+ rocket-x 50))
       (<= rocket-y (:y bonus) (+ rocket-y 50)))
     (and
       (<= rocket-x (:x bonus) (+ rocket-x 50))
       (<= rocket-y (+ (:y bonus) 40) (+ rocket-y 50)))
     (and
       (<= rocket-x (+ (:x bonus) 40) (+ rocket-x 50))
       (<= rocket-y (+ (:y bonus) 40) (+ rocket-y 50)))))

(defn bonus-hit [state]
 (let [rocket-x (-> state :rocket :x)
       rocket-y (-> state :rocket :y)
       bonus (:bonus state)]
   (cond
    (empty? bonus) state
    (bonus-collision? rocket-x rocket-y bonus)
    (assoc state
     :score (+ (:score state) (:points bonus))
     :bonus {})
    :else state)))

(defn age-smoke [smoke]
  (update smoke :age + 0.3))

(defn old? [smoke]
  (< 2.0 (:age smoke)))

(defn remove-old-smokes [smokes]
  (remove old? smokes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; creation methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn create-meteor [state width]
 (if (= (rand-int 25) 1)
     (if-not (or
               (-> state :rocket :dir (= 0))
               (-> state :rocket :dir (= -1)))
             (update-in state [:meteors] conj {:x (rand-int (+ width -40)) :y -40 :speed (+ (rand-int 8) 5)})
             state)
     state))

(defn create-smoke [x y]
  {:pos [(+ x 25 (- (rand-int 10) 5))
         (+ y 50 (- (rand-int 10) 5))]
   :dir 0.0
   :age 0.0
   :col [(+ (rand-int 105) 150)
         (+ (rand-int 100) 100)
         (rand-int 100)]})

(defn emit-smoke [state]
  (let [x (-> state :rocket :x)
        y (-> state :rocket :y)]
    (update-in state [:smoke] conj (create-smoke x y))))

(defn create-new-star [state width]
 (if (= (rand-int 7) 1)
   (if-not (or
             (-> state :rocket :dir (= 0))
             (-> state :rocket :dir (= -1)))
           (assoc state
            :stars (conj (:stars state) (create-star 1 width)))
           state)
   state))


(defn create-bonus [state width height]
 (if (and (empty? (:bonus state)) (= (rand-int 100) 1))
   (if-not (or
             (-> state :rocket :dir (= 0))
             (-> state :rocket :dir (= -1)))
           (if (= (rand-int 5) 1)
             (assoc state :bonus {:x (rand-int (+ width -40)) :y -40 :points 25 :speed 3 :image "images/bonus2.png"})
             (assoc state :bonus {:x (rand-int (+ width -40)) :y -40 :points 10 :speed 2 :image "images/bonus.png"}))
     state)
  state))


(defn fly-backwards [smoke state]
 (if (-> state :rocket :dir (= 2))
     []
     smoke))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; reset methods;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset-game [state width height]
 (let [{:keys [x y]} (:rocket state)]
  (if (inside? x y width height)
    (reset-state-variable state)
    state)))

(defn reset-game-over [gameOver state]
  (if (-> state :rocket :dir (not= 0))
    false
    true))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

;;;;;;;; move methods;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn move [state event]
  (case (:key event)
    (:w :up) (assoc-in state [:rocket :dir] 1)
    (:s :down) (assoc-in state [:rocket :dir] 2)
    (:a :left) (assoc-in state [:rocket :dir] 3)
    (:d :right) (assoc-in state [:rocket :dir] 4)
    state))

(defn move-meteors [meteor]
 (let [speed (:speed meteor)]
  (update-in meteor [:y] #(+ % speed))))

(defn move-star [star]
 (update-in star [:y] #(+ % (:speed star))))

(defn move-stars [state]
 (if-not (or
           (= (:dir (:rocket state)) 0)
           (= (:dir (:rocket state)) -1))
         (assoc state
          :stars (map move-star (:stars state)))
         state))

(defn move-bonus [state]
 (let [bonus (:bonus state)]
  (if (empty? bonus)
    state
    (assoc state
     :bonus (update bonus :y + 2)))))

(defn move-rocket [rocket]
 (case (:dir rocket)
  1 (update-in  rocket [:y] - 5)
  2 (update-in  rocket [:y] + 5)
  3 (update-in  rocket [:x] - 5)
  4 (update-in  rocket [:x] + 5)
  0 (update-in  rocket [:x] + 0)
  -1 (update-in  rocket [:x] + 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; draw method
(defn draw [state]
  (q/background 0)
  (q/fill 250 250 250)
  (q/stroke 250 250 250)

  (doseq [star (:stars state)]
   (cond
    (= (:color star) 0) (do
                         (q/fill 250 250 250)
                         (q/stroke 250 250 250)
                         (q/ellipse (:x star) (:y star) (:size star) (:size star)))
    (= (:color star) 1) (do
                          (q/fill 255 255 26)
                          (q/stroke 255 255 26)
                          (q/ellipse (:x star) (:y star) (:size star) (:size star)))
    :else (do
            (q/fill 255 77 77)
            (q/stroke 255 77 77)
            (q/ellipse (:x star) (:y star) (:size star) (:size star)))))

  (let [bonus (:bonus state)]
   (when-not (empty? bonus)
    (q/image (q/load-image (:image bonus))
             (:x bonus)
             (:y bonus)
             40 40)))

  (q/image (:image (:rocket state))
           (:x (:rocket state))
           (:y (:rocket state)))
  (q/fill 0 0 255)
  (q/text-align :left)
  (q/stroke 0 0 255)

  (doseq [meteor (:meteors state)]
    (q/image (q/load-image "images/meteor.png")
             (:x meteor)
             (:y meteor)))

  (doseq [smoke (:smoke state)]
    (let [age (:age smoke)
          size (max 0.0 (- 10.0 (* 5.0 age)))
          [r g b] (:col smoke)
          [x y] (:pos smoke)]
      (q/fill 0 0 250 150)
      (q/stroke 0 0 250 150)
      (q/ellipse x y size size)))

  (q/fill 255 255 255)
  (q/text-size 20)
  (q/text (str "Score: " (:score state)) 10 30)
  (q/text (str "Highscore: " (:highscore state)) (- (q/width) 140) 30)
  (q/fill 200 0 0)
  (q/text-font (q/create-font "DejaVu Sans" 40 true))
  (q/text-align :center)
  (when (:game-over state)
    (q/text (str "Game Over...\nMove to try again") (/ (q/width) 2) 500)))

; update method
(defn update-state [state]
 (-> state
  (update-in [:meteors] (fn [meteors] (map move-meteors meteors)))
  (update-in [:rocket] move-rocket)
  move-bonus
  move-stars
  (create-new-star (q/width))
  (update-in [:stars] remove-stars (q/height))
  emit-smoke
  (update-in [:smoke] (fn [smokes] (map age-smoke smokes)))
  (update-in [:smoke] remove-old-smokes)
  (meteor-out (q/height))
  (create-meteor (q/width))
  (bonus-out (q/height))
  (create-bonus (q/width) (q/height))
  bonus-hit
  meteor-hit
  (reset-game (q/width) (q/height))
  (update-in [:smoke] fly-backwards state)
  (update-in [:game-over] reset-game-over state)))

;; defsketch, fun-mode = functional mode
(q/defsketch rocket_game
  :host "host"
  :title "rocket game"
  :size [600 700]
  :setup setup
  :draw draw
  :key-pressed move
  :update update-state
  :middleware [m/fun-mode])
