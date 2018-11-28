(ns proyecto-final.core
  (:require [proyecto-final.data2 :as data]
            [cortex.nn.network :as network]
            [cortex.nn.layers :as layers]
            [cortex.nn.execute :as execute]
            [cortex.optimize.adam :as adam]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(defonce all-data (-> (data/load-data)
                  (shuffle)
                  (data/train-validation-split 0.70)))
(def num-nodes 32)

(def network-architecture
  [(layers/input 5 1 1 :id :x)

   (layers/linear num-nodes)
   (layers/relu)

   (layers/linear num-nodes)
   (layers/relu)

   (layers/linear 1 :id :y)])

(def starting-network {:network  (network/linear-network
                                  network-architecture)
                       :optimizer (adam/adam :alpha 0.01)})

(defn validate
  "Validación de la red"
  [cur-network]
  (println "Resultados:" (pr-str (data/stats (:network cur-network) (:validation all-data))))
  )

(defn train
  "Entrenamiento de la red"
  [incoming-network epoch-count]
  (loop [{:keys [network optimizer] :as cur-network} incoming-network
          epoch 0]


    (if (zero? (mod epoch 10))
      (println "Epoch: " epoch (pr-str (data/stats network (:train all-data)))))

    (if (> epoch-count epoch)
      (recur (execute/train network (:train all-data) :optimizer optimizer) (inc epoch))
      cur-network))
  )

(defn -main 
  [& args]
  (println "Entrenamiento...")
  (let [r1 (train starting-network 100)] 
    (validate r1)
    )
  )
