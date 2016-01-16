(ns plant-maker.core
  (:use seesaw.core
        seesaw.graphics
        seesaw.color
        seesaw.keystroke
        clojure.math.numeric-tower)
  (:require [clojure.core.matrix :as m]
            clojure.core.matrix.operators)
  (:gen-class))

;;(set-current-implementation :vectorz) 

(native!)

;; modes: start, mdeolOld, modelNew off
(def mouse-mode (atom :start))

(def components (atom '()))

(def model (atom {:start [0 0] :end [100 100] :components '()}))

(def stalk-shoot (atom false))

(defn line-style [c]
  (style :foreground c :background c))


(def cnv (canvas :id :cnv :background "#FFF"
                 :size [750 :by 700]
                 :paint (fn [c g]
                          ;;(println "components = " @components)
                          (doseq [{s :stage [x1 y1] :start  [x2 y2] :end} @components]
                            (let [col (case s
                                        :old "#000"
                                        :new "#0f0"
                                        :model "#00f"
                                        nil)]
                               ;;(println "painting...")
                               (draw g (line x1 y1 x2 y2) (line-style col))
                               (case @mouse-mode
                                 (:modelOld :modelNew)
                                 (draw g (circle x1 y1 2) (line-style col)
                                       (circle x2 y2 2) (line-style col))
                                 nil))))))

                              
(defn distance [p1 p2]
  (let [diff (map - p1 p2)]
    (sqrt (transduce (map #(* % %)) + diff))))
                 

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

(defn make-dim-fun []
  (let [totd (distance (:start @model) (:end @model))]
    (fn [x]
      (let [powsum (atom 0)]
        (doseq [{s :stage p1 :start  p2 :end} (:components @model)]
          (let [power (if (= s :new) x 1)]
            (swap! powsum + (expt (distance p1 p2) power))))
        (- @powsum (expt totd x))))))
            
(defn calcDim []
  (find-root (make-dim-fun) 1 2 0.001))

(defn add-to-model [mod c]
    (assoc mod :components (conj (:components mod) c)))

(defn add-comp-to-model [newcomp]
  ;;(println "Here!")
  (swap! components conj newcomp)
  (repaint! cnv)
  (swap! model add-to-model newcomp))


(defn make-mouse-fun [& args]
  (let [prev (atom '())]
    (fn [e]
      ;;(println "components = " @components)
      ;;(println @mouse-mode)
      ;;(println "prev = " @prev)
      (if (= e :reset)
        (reset! prev '())
        (let [x (.getX e)
              y (.getY e)]
          (if (> (count @prev) 0)
            (case @mouse-mode
              :start (do
                       ;;(println "start")
                       (reset! components (list {:start (vec @prev) :end [x y] :stage :model}))
                       (repaint! cnv)
                       (swap! model assoc :start (vec @prev) :end [x y])
                       (reset! prev '())
                       )
              (:modelNew :modelOld) (let [stage (if (=  @mouse-mode :modelNew) :new :old)
                                          newcomp {:start (vec @prev) :end [x y] :stage stage}]
                                      ;;(println "model")
                                      (add-comp-to-model newcomp)
                                      ;; (if (and @stalk-shoot (= @mouse-mode :modelOld))
                                      ;;   (let [newercomp {:start (vec @prev) :end [x y] :stage :new}]
                                      ;;     (add-comp-to-model newercomp)))
                                      (reset! prev '()))
              ;;(println "case failed")
              )
            (do
              ;;(println "prev empty")
              (reset! prev (list x y))
              (if (= @mouse-mode :start)
                (reset! components '()))))
          ;;(println "model: " @model)
          (repaint! cnv))))))

(def mousefunction (make-mouse-fun))

(defn clear []
  (reset! components '())
  (repaint! cnv)
  ;;(config! cnv :listen [:mouse-clicked (make-mouse-fun)])
  (mousefunction :reset))

(def rbs (list 
          (radio :id :stalk-shoot-false
                 :listen [:action (fn [e]
                                    (reset! stalk-shoot false))]
                  :text "False")
           (radio :id :stalk-shoot-false
                 :listen [:action (fn [e]
                                    (reset! stalk-shoot true))]
                  :text "True")))

(def radios (button-group))

(config! rbs :group radios)

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

(defn apply-transform-to-map [fun cmp]
  (let [res (transient {})]
    (doseq [[k v] cmp]
           (assoc! res k
                  (if (not (= k :stage))
                    (fun v)
                    v)))
    res))

(defn apply-transform [func cmps]
  (map #(apply-transform-to-map func %) cmps))

(defn next-gen []
  (let [res (atom '())]
    (doseq [{s :stage a :start b :end :as c} @components]
      ;;(println "iterating")
      ;;(println "components = " @components)
      (if (= s :old)
        (swap! res conj c)
        (let [{m1 :start m2 :end} @model
              f (make-transform [m1 m2] [a b])
              newclist (apply-transform f (:components @model))]
          (swap! res concat newclist))))
    ;;(println "res  = " @res)
    (reset! components @res)
    (repaint! cnv)))
  

(def f (frame :title "Plant Grower"
              :on-close :exit
              ))

(def controls
  (horizontal-panel :items [;;(horizontal-panel :items (conj rbs
                              ;;                             (label "Stalk => Shoot?")))
                            ;;(label "   ")
                            (button :id :setModel
                                    :text "Set Start"
                                    :listen [:action (fn [e]
                                                       (reset! mouse-mode :modelOld))])
                            (button :id :setOld
                                    :text "Set Stalks"
                                    :listen [:action (fn [e]
                                                       (reset! mouse-mode :modelNew))])
                            (button :id :setNew
                                    :text "Set Shoots"
                                    :listen [:action (fn [e]
                                                       (swap! components
                                                              (fn [l]
                                                                (filter #(not
                                                                          (= (:stage %)
                                                                             :model)) l)))
                                                       (reset! mouse-mode :off))])
                            (button :id :newxtGen
                                    :text "Next!"
                                    :listen [:action (fn [e]
                                                       (next-gen))])
                            (button :id :clear
                                    :text "Clear"
                                    :listen [:action (fn [e]
                                                       (reset! components '())
                                                       (reset! model {:stage :new :start [0 0] :end [100 100] :components '()})
                                                       (reset! mouse-mode :start)
                                                       (repaint! cnv))])
                            (button :id :dimButt
                                    :text "Dimension: "
                                    :listen [:action (fn [e]
                                                       (text! (select f [:#dimField])
                                                              (str (calcDim))))])
                            (text :id :dimField
                                  :text "")
                            ]))


(config! f :content
         (border-panel :hgap 5 :vgap 5 :border 5
                       :center cnv 
                       :south  controls))

(listen cnv :mouse-clicked mousefunction)
        
(defn -main [& args]
  (-> f pack! show!)
)

