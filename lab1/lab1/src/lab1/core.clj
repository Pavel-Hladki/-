(ns lab1.core
  (:gen-class))
(use 'clojure.java.io)

(def Ra 3)
(def Rb (* 1.5 Ra))
(def EMax 0.5)
(def EMin 0.15)


(defn to-point-obj 
	[point ind]
	{:index ind, :point (into [] point)})

(defn parse-point
  [pointStr ind]
  (let [points (clojure.string/split pointStr #",")]
  	(to-point-obj (map (fn [point]
  		(if (re-find #"\d+" point)
			(read-string point)))
  	    points) ind)))

(defn parse-coordinates
	[string]
	(let [coordStr (clojure.string/split string #" ")]
		 (reduce (fn [coordinates coord]
		 	(conj coordinates (parse-point (apply str coord) (count coordinates)))) 
		 	[] coordStr)))

(defn potential
  [distance]
  (Math/exp (- (* (/ 4 (* Ra Ra)) distance))))

(defn revised-potential
  [distance]
  (Math/exp (- (* (/ 4 (* Rb Rb)) distance))))

(defn euclidian-distance
  [point1 point2]
  (->> (map - point1 point2) (map #(* % %)) (reduce +)))

(defn hamming-distance
  [point1 point2]
  (count (filter true? (map not= point1 point2))))

(defn calculate-point-potential
	[point points distanceAlgorithm]
	(assoc point :distance (reduce #(+ %1 (potential (distanceAlgorithm (:point point)  (:point %2)))) 0 points)))

(defn calculate-potentials
	[points distanceAlgorithm]
	(map #(calculate-point-potential %1 points distanceAlgorithm) points))

(defn calculate-point-reviced-potential
	[point hightestPotentialPoint distanceAlgorithm]
	(assoc point :distance (- (:distance point) (* (:distance hightestPotentialPoint) (+ 0 (revised-potential (distanceAlgorithm (:point point) (:point hightestPotentialPoint)))) ))))

(defn calculate-reviced-potentials
	[hightestPotentialPoint points distanceAlgorithm]
	(map #(calculate-point-reviced-potential %1 hightestPotentialPoint distanceAlgorithm) points))
	
(defn get-min-value
	[values]
	(reduce min values))

(defn sort-points-by-distance-desc
	[points]
	(sort-by :distance > points)
	)

(defn get-point-with-min-distance-to-center
	[center points distanceAlgorithm]
	(get-min-value((map #(distanceAlgorithm center %) points))))
(defn print-result
	[centers]
	(apply #(println "index: "(str(:index %)) " - point: " (str (:point %))) centers) 
	)

(defn calculate-centers
	[points distanceAlgorithm]
	(let [potentialesMap (sort-points-by-distance-desc (calculate-potentials points distanceAlgorithm))]
       (loop [firstCenter (first potentialesMap) centers (conj [] (first potentialesMap)) restPotentiales(rest potentialesMap)]
		(let [revicedPotentialesMap (calculate-reviced-potentials (first centers) restPotentiales distanceAlgorithm)]	
	      	(let [nextCenter (first revicedPotentialesMap)] 
		          (if (> (:distance nextCenter) (* EMax (:distance firstCenter))) 
		            (recur firstCenter (conj centers nextCenter) (rest revicedPotentialesMap)) (if (< (:distance nextCenter) (* EMax (:distance firstCenter)))
		              (print-result centers) (if (>= (+ (/ (get-point-with-min-distance-to-center nextCenter revicedPotentialesMap distanceAlgorithm) Ra) 
		                          (/ (:distance nextCenter) (:distance firstCenter) ) ) 1)
		                          (recur firstCenter (conj centers nextCenter) (rest revicedPotentialesMap))
		                          (recur firstCenter centers (conj (rest revicedPotentialesMap) (assoc nextCenter :distance 0)))
		                        )
		           )
		        )
	      	)
	      )
	    )
	(println "The End!)...")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [coordinates (parse-coordinates (slurp (first args))) distanceAlgorithm (last args)]
  		(if (= (last args) "hamm") (calculate-centers coordinates euclidian-distance) (if (= (last args) "eucl")
  			(calculate-centers coordinates euclidian-distance)))
   )
)
