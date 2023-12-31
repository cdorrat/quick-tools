(ns samples.data.csv
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]))

(defn read-csv []
  (with-open [reader (io/reader "./resources/iris.csv")]
    ;; read-csv returns a lazy sequence of string vectors
    ;; make sure you dont hold on to the head for large files
    (->> (csv/read-csv reader)
         (drop 1) ;; skip header row
         (map println)         
         (take 10)
         dorun))) ;; <= make sure to consume the sequence within the with-open 

(defn read-csv-as-maps []
  (with-open [reader (io/reader "./resources/iris.csv")]
    (let [data (csv/read-csv reader)]
      (->>
       ;; this gives us a map per row (still lazy)
       (map zipmap
            (->> data  first (map keyword) repeat)
            (rest data))
       (take 10)
       doall))))
