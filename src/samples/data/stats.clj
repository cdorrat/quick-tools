(ns samples.data.stats
  "Uses Kixi for stats, see https://github.com/MastodonC/kixi.stats"
  (:require
   [kixi.stats.core :as s]
   [kixi.stats.distribution :as sd]
   [redux.core :refer [fuse]])
  )

(def sample-data (repeatedly 100
                             (fn []
                               {:x (rand-int 100 )
                                :y (+ 100 (rand-int 200)) 
                                :z (* 5 (rand))})))



(defn descriptive-stats []
  (transduce identity (fuse {:min s/min
                             :max s/max
                             :mean s/mean
                             :median s/median
                             :sd s/standard-deviation})
             (map :x sample-data)))

(defn counting-classes []
  (let [gt50? (filter #(> % 50))]
   (transduce identity (fuse {:count s/count
                              :gt5 (gt50? s/count)})
              (map :x sample-data))))

(defn distibutions []
  (let [dist (transduce identity s/histogram (map :y sample-data))]
    {:lower-2.5 (sd/quantile dist 0.025) ;; 2.5 quintile
     :95 (sd/quantile dist 0.95)
     :99 (sd/quantile dist 0.99)
     :999 (sd/quantile dist 0.999)
     }))
