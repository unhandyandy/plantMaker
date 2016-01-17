(ns plant-maker.core-test
  (:require [clojure.test :refer :all]
            [plant-maker.core :refer :all]))

(defn fixture-simple [f]
  (reset! model 
          {:start [206 573], :end [564 312], :components (list {:start [463.0 453.0], :end [564.0 311.99999999999994], :stage :new} {:start [298.79259633069677 385.39025328490754], :end [463 453], :stage :new} {:start [206.6529485187355 572.523967700028], :end [298 387], :stage :old})})
  (reset! components
          (list {:start [463.0 453.0], :end [564.0 311.99999999999994], :stage :new} {:start [298.79259633069677 385.39025328490754], :end [463 453], :stage :new} {:start [206.6529485187355 572.523967700028], :end [298 387], :stage :old}))
  (f)
  )

(use-fixtures :each fixture-simple)
          
(deftest iterate-2d 
  (let [nnew (count (filter #(= (:stage %) :new) @components))
        nold (count (filter #(= (:stage %) :old) @components))
        nmodel (count (:components @model))]
    (next-gen)
    (let [nnext (count @components)]
      (is (= nnext (+ nold (* nnew nmodel)))))))

