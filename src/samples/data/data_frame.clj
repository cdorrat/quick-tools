(ns samples.data.data-frame
  "using tech.ml.dataset as a Pandas equivalent, see https://github.com/techascent/tech.ml.dataset?tab=readme-ov-file"
  (:require
   [tech.v3.dataset :as ds]
   [tech.v3.libs.fastexcel] ;; required for xls support
   ))


(defn from-csv []
  (ds/->dataset "./resources/iris.csv"))

(defn from-internet []
  (ds/->dataset "https://github.com/techascent/tech.ml.dataset/raw/master/test/data/ames-train.csv.gz"))

(defn from-excel []
  (ds/->dataset "https://github.com/techascent/tech.ml.dataset/raw/master/test/data/file_example_XLSX_1000.xlsx"))


(defn descriptive-stats []
  (let [data  (from-csv)]
   (ds/descriptive-stats data) ;; returns a data-frame of descriptive stats, 1 row per column
   (ds/brief data)))  ;; descriptive stats as clojure maps 

(defn filtering-rows []
  (let [ames-ds (from-internet)]
     (-> ames-ds
          (ds/filter #(< 30000 (get % "SalePrice")))
          (ds/select ["OverallQual" "SalePrice"] (range 5)))
     ;; or select using a single column
      (-> ames-ds
          (ds/filter-column "SalePrice" #(< 30000 %))
          (ds/select ["OverallQual" "SalePrice"] (range 5)))
     ))

  
(defn group-by-stats []
  (let [stock-ds (ds/->dataset "./resources/stocks.csv")]
    (->> (ds/group-by-column stock-ds "symbol") ;; split into map of col-name -> dataset
         (map (fn [[k v]] (ds/descriptive-stats v)))) ;; calculate stats for each stock
    ))


(defn showing-metadata []
  ;; colums have attached meta-data
  (map meta (ds/columns (from-excel))))
