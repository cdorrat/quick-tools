(ns samples.data.db
  "Samples for reading from databases"
  (:require
   [next.jdbc :as jdbc])
  )


;; sample database specs
{:dbtype "sqlite" :dbname "./resources/sample.db"}
{:dbtype "mysql" :dbname "sample"
 :user "admin"
 :password "secret"}
{:jdbcUrl "jdbc:snowflake://xy12345.snowflakecomputing.com/?user=peter&warehouse=mywh&db=mydb&schema=public" }


(defn get-datasource []
  (jdbc/get-datasource {:dbtype "sqlite" :dbname "./resources/sample.db"}))

(defonce ds (get-datasource))

(defn setup-sample-database [ds]
  (jdbc/execute-one! ds ["
create table invoice (
  id integer primary key,
  product varchar(32),
  unit_price decimal(10,2),
  unit_count int unsigned,
  customer_id int unsigned
)"])
  
  (jdbc/execute-one! ds ["
insert into invoice (product, unit_price, unit_count, customer_id)
values ('apple', 0.99, 6, 100),
       ('banana', 1.25, 3, 100),
       ('cucumber', 2.49, 2, 100)
"])
  )

(defn query-samples [ds]
  ;; get a list of namespace qualified maps 
  (jdbc/execute! ds ["select * from invoice"])

  ;; return a single record
  (jdbc/execute-one! ds ["select * from invoice where id = 2"])

  ;; reduce over a dataset
  (reduce (fn [name-set row]
            (conj name-set (:product row)))
          #{}
          (jdbc/plan ds ["select * from invoice where customer_id = ?" 100]))

  ;; extract 2 columns
  (into []
        (map (juxt :product :unit_price))
        (jdbc/plan ds ["select * from invoice where customer_id = ?" 100]))

  ;; process rows for side effects
  (run! #(println (:product %))
        (jdbc/plan ds ["select * from invoice where customer_id = ?" 100]))
  )
