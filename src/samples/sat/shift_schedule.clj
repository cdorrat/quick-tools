(ns samples.sat.shift-schedule
  "Using a SAT solver to schedule a roster"
  (:import
   [com.google.ortools Loader]
   [com.google.ortools.sat CpModel CpSolver CpSolverStatus IntVar LinearExpr]
   ))



(def staff
  [{:id 1 :name "Janet Hoffman" :hours 40 :rate 32}
   {:id 2 :name "Bing Alfronso" :hours 40 :rate 35}
   {:id 3 :name "Sam Beucamp" :hours 40 :rate 37}
   {:id 4 :name "Alice Morningstar" :hours 40 :rate 30}
   {:id 5 :name "Frederick Johnson" :hours 40 :rate 32}
   {:id 6 :name "Mary Nightingdale" :hours 20 :rate 34}
   {:id 7 :name "Tim Johansen" :hours 20 :rate 33}
   {:id 8 :name "Michael Casual" :hours 0 :rate 40}
   {:id 9 :name "Katrina Casual" :hours 0 :rate 42}
   {:id 11 :name "Jordan Casual" :hours 0 :rate 48}
   {:id 12 :name "Agency 1" :hours 0 :rate 48}
   {:id 13 :name "Agency 2" :hours 0 :rate 48}
   {:id 14 :name "Agency 3" :hours 0 :rate 48}
   {:id 15 :name "Agency 4" :hours 0 :rate 48}
   {:id 16 :name "Agency 5" :hours 0 :rate 48}
   {:id 17 :name "Agency 6" :hours 0 :rate 48}
   {:id 18 :name "Agency 7" :hours 0 :rate 48}
   {:id 19 :name "Agency 8" :hours 0 :rate 48}
   {:id 21 :name "Agency 9" :hours 0 :rate 48}
   {:id 22 :name "Agency 10" :hours 0 :rate 48}
   {:id 23 :name "Agency 11" :hours 0 :rate 48}
   {:id 24 :name "Agency 12" :hours 0 :rate 48}
   ]
  )

(def shifts
  [
   {:id 1 :day :monday :time :morning  :staff 2 :hours 8}
   {:id 2 :day :monday :time :afternoon :staff 2 :hours 8}
   {:id 3 :day :monday :time :night :staff 1 :hours 10}
   {:id 4 :day :tuesday :time :morning  :staff 2 :hours 8}
   {:id 5 :day :tuesday :time :afternoon :staff 2 :hours 8}
   {:id 6 :day :tuesday :time :night :staff 1 :hours 10}
   {:id 7 :day :wednesday :time :morning  :staff 2 :hours 8}
   {:id 8 :day :wednesday :time :afternoon :staff 2 :hours 8}
   {:id 9 :day :wednesday :time :night :staff 1 :hours 10}
   {:id 10 :day :thursday :time :morning  :staff 3 :hours 8}
   {:id 11 :day :thursday :time :afternoon :staff 3 :hours 8}
   {:id 12 :day :thursday :time :night :staff 1 :hours 10}
   {:id 13 :day :friday :time :morning  :staff 4 :hours 8}
   {:id 14 :day :friday :time :afternoon :staff 4 :hours 8}
   {:id 15 :day :friday :time :night :staff 2 :hours 10}      
   ]
  )

(defn shift-name [{:keys [day time] :as shift}]
  (format "shift_%s_%s" (name day) (name time)))


(defn model-vars [shift-vars]
  (into-array IntVar (map :model-var shift-vars)))

;; ===================================================================================================
;; create the model with constraints 
(defn create-model [staff shifts]
  (let [model (CpModel.)
        ;; create 1 boolean var per shift & staff member
        shifts-vars (for [shift shifts employee staff]
                      {:shift shift 
                       :employee employee
                       :model-var (.newBoolVar model (str (shift-name shift) "_" (:id employee)))})]

    ;; add constraints
    
    ;; each employee works at exactly their scheduled hours
    (doseq [[emp vars] (filter (fn [[emp vars]] (< 0 (:hours emp)))
                               (group-by :employee shifts-vars))]
      (.addEquality model
                    ;; sum of shifts_worked * shift_hours
                    (LinearExpr/weightedSum 
                     (model-vars vars) 
                     (long-array (map (comp :hours :shift) vars)))
                    (:hours emp)))

    ;; each employee can only work 1 shift per day
    (doseq [[emp vars] (group-by (juxt :employee (comp :day :shift)) shifts-vars)]
      (.addLessOrEqual model (LinearExpr/sum (model-vars vars)) 1))
    
    ;; each shift must have at least the minimum number of people working
    (doseq [[shift vars] (group-by :shift shifts-vars)]
      (.addGreaterOrEqual model (LinearExpr/sum (model-vars vars)) (:staff shift)))

    ;; minimize the payroll cost
    (.minimize model (LinearExpr/weightedSum
                      (model-vars shifts-vars)
                      (long-array (map #(* (-> % :shift :hours) (-> % :employee :rate)) shifts-vars))))
       
    {:model model
     :shift-vars shifts-vars}))

;; ===================================================================================================
;; output the results 
(def shift-id {:morning 1 :afternoon 2 :night 3})
(def day-id {:monday 0 :tuesday 1 :wednesday 2 :thursday 3 :friday 4 :saturday 5 :sunday 6})

(defn print-hours-by-employee [staff solver shift-vars]
  (let [default-hours (zipmap (map :id staff)
                            (map #(assoc % :min-hours (:hours %) :hours 0 :num-shifts 0) staff))
        
        staff-hours (reduce (fn [m {:keys [model-var employee shift]}]
                              (let [solver-val (.value solver model-var)]
                               (-> m
                                   (update-in  [(:id employee) :hours] + (* solver-val (:hours shift)))
                                   (update-in  [(:id employee) :num-shifts] + solver-val))))
                            default-hours shift-vars)]

    (println "\n** Hours by staff member")
    (doseq [{:keys [id name hours min-hours rate num-shifts]} (sort-by :id (vals staff-hours))]
      (println id "-" name " - " num-shifts "[" hours "/" min-hours "] @" rate))))

(defn print-shifts [ solver shift-vars]
  (doseq [[shift model-vars] (sort-by (juxt (comp day-id :day first) (comp shift-id :time first))
                                      (group-by :shift  shift-vars))]
    (println "Shift: " (-> shift :day name) "-" (-> shift :time name))
    (doseq [{:keys [employee]} (filter #(= 1 (.value solver (:model-var %))) model-vars)]
      (println "  " (:name employee)))))

(defn print-solution [staff solver shift-vars]
  (print-shifts solver shift-vars)
  (print-hours-by-employee staff solver shift-vars))

;; ===================================================================================================
;; run the model
(defn run-model []
  (Loader/loadNativeLibraries)
  (let [{:keys [model shift-vars]} (create-model staff shifts)
        solver (CpSolver.)
        status (.solve solver model)]
    (if (#{CpSolverStatus/OPTIMAL CpSolverStatus/FEASIBLE} status)
      (print-solution staff solver shift-vars)
      (println "No solution found"))))
