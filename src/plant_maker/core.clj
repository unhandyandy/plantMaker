(ns plant-maker.core
  (:use seesaw.core
        seesaw.graphics
        seesaw.color
        seesaw.keystroke
        seesaw.chooser
        clojure.math.numeric-tower)
  (:require [clojure.core.matrix :as m]
            clojure.core.matrix.operators
            clojure.edn)
  (:gen-class))

;;(set-current-implementation :vectorz) 

(native!)

;; modes: start, mdeolOld, modelNew off
(def mouse-mode (atom :start))

;; the previous mouse-mode
(def mouse-mode-prev (atom :start))

;; the objects that will be drawn on the canvas
(def components (atom '()))

;; the recursor for a shoot
(def model (atom {:start [0 0] :end [100 100] :components '()}))

;;(def stalk-shoot (atom false))

;; sets fg and bg to color c
(defn line-style [c]
  (style :foreground c :background c))

;; sets radius of attraction for mouse
(def gravity-radius 16)

;; separation between grid points
(def grid-radius (atom 180))

;; size at which recursion stops
(def resolution 6)

;; calculate distance between points, specified as vectors of xy coordinates  
(defn distance [p1 p2]
  (let [diff (map - p1 p2)]
    (sqrt (transduce (map #(* % %)) + diff))))

;; draws arrow to indicate orientation at midtpoint of segment from a to b on
;; graphics object g in color c
(defn draw-arrow [a b g color]
  (if (> (distance a b) 100)
    (let [[xm ym :as m] (map round (map #(* 0.5 %) (map + a b)))
          [xd yd :as dir] (m/normalise (map - b a))
          perp [(- yd) xd]
          [xa1 ya1] (map + (map - m (map #(* 20 %) dir)) (map #(* 10 %) perp))
          [xa2 ya2] (map - (map - m (map #(* 20 %) dir)) (map #(* 10 %) perp))]
      (draw g (line xa1 ya1 xm ym) (line-style color))
      (draw g (line xa2 ya2 xm ym) (line-style color)))))

;; create the canvas
(def cnv (canvas :id :cnv :background "#FFF"
                 :size [750 :by 700]
                 ;; called by (repaint! cnv)
                 :paint (fn [c g]
                          ;;(println "components = " @components)
                          (doseq [{s :stage [x1 y1] :start  [x2 y2] :end} @components]
                            (let [col (case s
                                        :old "#000"
                                        :new "#0f0"
                                        :model "#00f"
                                        :point "#777"
                                        nil)]
                              ;;(println "painting...")
                              (draw g (line x1 y1 x2 y2) (line-style col))
                              (if (= s :new)
                                (draw-arrow [x1 y1] [x2 y2] g col))
                              ;; draws grid points only defining recursor
                              (case @mouse-mode
                                (:modelOld :modelNew :start)
                                (if (= s :point)
                                  (draw g (circle x1 y1 2) (line-style col)))
                                nil))))))

;; a point is line segment in with equal endpoints
(defn make-point [p]
  {:start p :end p :stage :point})

(def sqrt3 (sqrt 3))

;; make the points of a rectangular grid with separation d
(defn make-grid-rect [d]
  (let [res (atom '())]
    (doseq [x (range 0 (.getWidth  cnv) d)
          y (range 0 (.getHeight cnv) d)]
      (swap! res conj (make-point [x y])))
    @res))

;; make the points of a hexagonal grid with separation d
(defn make-grid-hex [d]
  (let [res (atom '())
        w (round (/ d 2))
        h (round (* sqrt3 d))]
    (doseq [x (range w (.getWidth  cnv) d)
          y (range 0 (.getHeight cnv) h)]
      (swap! res conj (make-point [x y])))
    (doseq [x (range 0 (.getWidth  cnv) d)
            y (range (round (/ h 2)) (.getHeight cnv) h)]
      (swap! res conj (make-point [x y])))
    @res))

;; call (grid-fun to generate grid points
(def grid-fun (atom #(make-grid-rect @grid-radius)))

;; toggle between rect and hex grids
(def toggle-grid
  (let [pattern (atom :rect)]
    (fn [e]
      (if (= @pattern :rect)
        (do 
          (reset! grid-fun #(make-grid-hex @grid-radius))
          (reset! pattern :hex))
        (do
          (reset! grid-fun #(make-grid-rect @grid-radius))
          (reset! pattern :rect))))))

;; find a root of f(x) where a<x<b, accurate to dx
(defn find-root [f a b dx]
  (let [fa (f a)
        fb (f b)
        g (* 0.5 (+ a b))
        fg (f g)]
    (loop [guess g
           lastguess (if (<= (* fa fg) 0)
                       a b)
           f1 fg
           f2 (f lastguess)]
      (if (< (abs (- guess lastguess)) dx)
        guess
        (let [newguess (* 0.5 (+ guess lastguess))
              fn (f newguess)
              [newlast fnl] (if (<= (* f1 fn) 0)
                              (list guess f1)
                              (list lastguess f2))]
          (recur newguess
                 newlast
                 fn fnl))))))

;; returns a function of one variable which is zero
;; when the argument is the correct dimension
(defn make-dim-fun []
  (let [totd (distance (:start @model) (:end @model))]
    (fn [x]
      (let [powsum (atom 0)]
        (doseq [{s :stage p1 :start  p2 :end} (:components @model)]
          (let [power (if (= s :new) x 1)]
            (swap! powsum + (expt (distance p1 p2) power))))
        (- @powsum (expt totd x))))))

;; find approximate dimension of fractal
(defn calcDim []
  (find-root (make-dim-fun) 1 2 0.001))

;; add compnent c to model mod (atom)
(defn add-to-model [mod c]
    (assoc mod :components (conj (:components mod) c)))

;; add newcomp to components and the model (recursor)
(defn add-comp-to-model [newcomp]
  ;;(println "Here!")
  (swap! components conj newcomp)
  (repaint! cnv)
  (swap! model add-to-model newcomp))

;; return distance and closest point from p to segment ab
(defn gravity-vars [p a b]
  (if (= a b)
    (list (distance p b) b)
    (let [v (map - b a)
          [xu yu :as u] (m/normalise v)
          uperp [(- yu) xu]
          delpa (map - p a)
          da (m/dot u delpa)
          db (m/dot u (map - b p))]
      ;; (if (<= da 0)
      ;;   (list (distance p a) a)
      ;;   (if (<= db 0)
      ;;     (list (distance p b) b)
      (list (abs (m/dot uperp delpa)) (map + a (map #(* da %) u)))
      ;; ))
      )))

;; return best match of point to all existing components
(defn best-gravity-vars [point]
  (loop [[head & tail] @components
         bestd 1000
         bestp nil]
    (if (nil? head)
      (list bestd bestp)
      (let [[dist pointprime] (gravity-vars point (:start head) (:end head))]
        (if (< dist bestd)
          (recur tail dist pointprime)
          (recur tail bestd bestp))))))

;; return gravtized point
(defn apply-gravity [point]
  (let [[d p] (best-gravity-vars point)]
    (map round (if (< d gravity-radius) p point))))



;; (defn clear []
;;   (reset! components grid)
;;   (repaint! cnv)
;;   (config! cnv :listen [:mouse-clicked (make-mouse-fun)])
;;   (mousefunction :reset))

;; (def rbs (list 
;;           (radio :id :stalk-shoot-false
;;                  :listen [:action (fn [e]
;;                                     (reset! stalk-shoot false))]
;;                   :text "False")
;;            (radio :id :stalk-shoot-false
;;                  :listen [:action (fn [e]
;;                                     (reset! stalk-shoot true))]
;;                   :text "True")))

;;(def radios (button-group))

;;(config! rbs :group radios)

;; return a function of a point [x y] which is a similarity transform
;; mapping vector [ma mb] to [a b]
(defn make-transform [[ma mb] [a b]]
  (let [[x y :as m1] (vec (map - mb ma))
        m2 [(- y) x]
        [xb yb :as p1] (vec (map - b a))
        p2 [(- yb) xb]
        mmat (m/matrix [m1 m2])
        xmat (m/matrix [p1 p2])
        tmat (m/mmul (m/inverse mmat) xmat)]
    (fn [invec]
      (let [transin (vec (map - invec ma))
            outvec (m/mmul (m/matrix transin) tmat)]
        (vec (map + a outvec))))))

;; apply transform fun to hash cmp,
;; representing a line segment
(defn apply-transform-to-map [fun cmp]
  (let [res (atom {})]
    (doseq [[k v] cmp]
      (swap! res assoc k
              (if (not (= k :stage))
                (fun v)
                v)))
    @res))

;; apply transform to collection of line segments (as hashes)
(defn apply-transform [func cmps]
  (map #(apply-transform-to-map func %) cmps))

;; take the existing components collection and recur once
(defn next-gen []
  (let [res (atom '())]
    (doseq [{s :stage a :start b :end :as c} @components]
      ;;(println "iterating")
      ;;(println "components = " @components)
      (if (= s :old)
        (swap! res conj c)
        (if (> (distance a b) resolution)
          (let [{m1 :start m2 :end} @model
                f (make-transform [m1 m2] [a b])
                newclist (apply-transform f (:components @model))]
            (swap! res concat newclist))
          (swap! res conj c))))
    ;;(println "res  = " @res)
    (reset! components @res)
    (repaint! cnv)))

;; change the center of the image to the current point newcenter
(defn recenter [newcenter]
  (let [fwidth (.getWidth  cnv)
        fheight (.getHeight cnv)
        oldcenter [(round (/ fwidth 2)) (round (/ fheight 2))]
        newcenteroff (vec (map #(+ 100 %) newcenter))
        oldcenteroff (vec (map #(+ 100 %) oldcenter))
        trans (make-transform [newcenter newcenteroff] [oldcenter oldcenteroff])]
    (swap! components #(apply-transform trans %))
    (reset! mouse-mode @mouse-mode-prev)
    (repaint! cnv)))

;; returns function to handle mouse clicks in canvas
(defn make-mouse-fun [& args]
  (let [prev (atom '())]
    (fn [e]
      ;;(println "components = " @components)
      ;;(println @mouse-mode)
      ;;(println "prev = " @prev)
      (if (= e :reset)
        (reset! prev '())
        (let [x0 (.getX e)
              y0 (.getY e)
              [x y] (apply-gravity [x0 y0])]
          (if (= @mouse-mode :recenter)
            (recenter [x y])
            (if (> (count @prev) 0)
              (do 
                (swap! components conj (make-point (vec @prev)))
                (case @mouse-mode
                  :start (do
                           ;;(println "start")
                           (reset! components (conj (@grid-fun) {:start (vec @prev) :end [x y] :stage :model}))
                           (repaint! cnv)
                           (swap! model assoc :start (vec @prev) :end [x y])
                           (reset! prev '())
                           )
                  (:modelNew :modelOld) (let [stage (if (=  @mouse-mode :modelNew) :new :old)
                                              newcomp {:start (vec @prev) :end [x y] :stage stage}]
                                          (swap! components conj (make-point [x y]))
                                          ;;(println "model")
                                          (add-comp-to-model newcomp)
                                          ;; (if (and @stalk-shoot (= @mouse-mode :modelOld))
                                          ;;   (let [newercomp {:start (vec @prev) :end [x y] :stage :new}]
                                          ;;     (add-comp-to-model newercomp)))
                                          (reset! prev '()))
                  ;;(println "case failed")
                  ))
              (do
                ;;(println "prev empty")
                (reset! prev (list x y))
                (if (= @mouse-mode :start)
                  (reset! components (conj (@grid-fun) (make-point @prev)))
                  (swap! components conj (make-point @prev))))))
          ;;(println "model: " @model)
          (repaint! cnv))))))

;; global variable denoting mouse click handler
(def mousefunction (make-mouse-fun))

;; create the frame for the app
(def f (frame :title "Plant Grower"
              ;:on-close :exit
              ))

;; create slider used to adjust searation in grid
(def grid-slider
  (slider :orientation :vertical
          :value 120
          :min 50
          :max 600
          :snap-to-ticks? true
          :paint-labels? true
          :major-tick-spacing 100
          :minor-tick-spacing 8))


;; handler for grid-slider
(listen grid-slider
        :change
        (fn [e]
          (reset! grid-radius (value grid-slider))))

;; handler for estart button
;; retains model but sets visisble components to initial shoot
(defn restart-fun [e]
  (reset! components
          (list {:stage :new
                 :start (:start @model)
                 :end (:end @model)}))
  (repaint! cnv))

;; save the current model to a file
(defn save-seed [e]
  (let [seedstr (prn-str @model)
        savefile (choose-file :type :save
                              ;;:filters ["Text" ["txt"]]
                              )]
    ;; repaint in case save window corrupted canvas 
    (repaint! cnv)
    (if savefile
      (spit savefile seedstr))))

;; load a model from a file
(defn load-seed [e]
  (let [loadfile (choose-file :type :open)]
    (if loadfile
      (let [seedstr (slurp loadfile)]
        (reset! model (clojure.edn/read-string seedstr))
        (restart-fun e)))))

;; control-frame is the control panel
;; control-frame refers to clear which refers to control-frame
;; so must be declared now
(declare control-frame)

;; handler for clear button
(defn clear [e]
  (reset! components (@grid-fun))
  (reset! model {:stage :new :start [0 0] :end [100 100] :components '()})
  (config! (select control-frame [:#dimField]) :text "")
  (reset! mouse-mode :start)
  (mousefunction :reset)
  (repaint! cnv))

;; define control panel
(def control-frame
  (frame :title "Controls"
         :content (vertical-panel
                   :items [;;(horizontal-panel :items (conj rbs
                           ;;                             (label "Stalk => Shoot?")))
                           ;;(label "   ")
                           ;; set initial shoot
                           (button :id :setModel
                                   :text "Set Start"
                                   :listen [:action (fn [e]
                                                      (reset! mouse-mode :modelOld))])
                           ;; set the stalks which are drawn but don't recurse
                           (button :id :setOld
                                   :text "Set Stalks"
                                   :listen [:action (fn [e]
                                                      (reset! mouse-mode :modelNew))])
                           ;; set shoots  which are drawn green and recurse
                           (button :id :setNew
                                   :text "Set Shoots"
                                   :listen [:action (fn [e]
                                                      (swap! components
                                                             (fn [l]
                                                               (filter #(case (:stage %)
                                                                          (:old :new) true
                                                                          false) l)))
                                                      (reset! mouse-mode :off))])
                           ;; recurse and draw
                           (button :id :newxtGen
                                   :text "Next!"
                                   :listen [:action (fn [e]
                                                      (next-gen))])
                           ;; recenter the image
                           (button :id :recenterButt
                                   :text "Recenter"
                                   :listen [:action (fn [e]
                                                      (reset! mouse-mode-prev @mouse-mode)
                                                      (reset! mouse-mode :recenter))])
                           ;; restart the construction from the initial shoot
                           ;; retaining existing model
                           (button :id :restart
                                   :text "Restart"
                                   :listen [:action restart-fun])
                           ;; calculate the dimension of the fractal
                           (button :id :dimButt
                                   :text "Dimension: "
                                   :listen [:action (fn [e]
                                                      (config! (select control-frame [:#dimField])
                                                               :text (str (calcDim))))])
                           ;; field to display the dimension
                           (label :id :dimField
                                  :text ""
                                  ;;:editable? false
                                  ;;:multi-line? false
                                  :size [100 :by 30])
                           ;; save button
                           (button :id :saveButt
                                   :text "Save Seed"
                                   :listen [:action save-seed])
                           ;; spacer
                           (label :text ""
                                  :size [100 :by 30])
                           ;; clears the image from the canvas
                           ;; and resets the model to empty
                           (button :id :clear
                                   :text "Clear"
                                   :listen [:action clear])
                           ;; toggles the grid shape
                           (button :id :setGrid
                                   :text "Toggle Grid"
                                   :listen [:action toggle-grid])
                           grid-slider
                           ;; load seed from file
                           (button :id :loadButt
                                   :text "Load Seed"
                                   :listen [:action load-seed])
                           ])))

;; (config! f :content
;;          (border-panel :hgap 5 :vgap 5 :border 5
;;                        :center (left-right-split
;;                                 controls cnv
;;                                 :divider-location 120)
;;                        ))

;; set the control-panel to always be on top of the canvas
(.setAlwaysOnTop control-frame true)

(config! f :content cnv)

(listen cnv :mouse-clicked mousefunction)

(config! f :on-close :exit)

(defn -main [& args]
  (-> f pack! show!)
  (-> control-frame pack! show!)
  (reset! components (@grid-fun))
  (repaint! cnv)
  )

