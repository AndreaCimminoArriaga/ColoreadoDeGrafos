;;Importing data from file 'data.lsp'
(load "data.lsp")

;;.______________________________________________________________________________________________________________.;;
;;|											 																	 |;;
;;| 		Initialice genetic algorithm parameters as described in the paper:									 |;;
;;|______________________________________________________________________________________________________________|;;


(defparameter *population_length* 50)
(defparameter *half_population_length* 25)
(defparameter *population* '())
(load "functions.lsp")
(defparameter *population* (generate_random_population))
(defparameter *generation* 0)
(defparameter *max_generations* 20000)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;|	max_generation_reached () -> This function returns true if the algorith has iterated over the maximum		 |;;
;;|								 number of generations as described in the paper								 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun max_generation_reached ()  (IF (= *max_generations* (EVAL (defparameter *generation* (+ *generation* 1) )) ) T NIL)  )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;|	pick_top_half_population () -> This frunction takes the first half number of chromosomes in the population	 |;;
;;|								   but only works if the number of the population is even.						 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- nthcar (n list)											    										 |;;
;;|______________________________________________________________________________________________________________|;;


(defun pick_top_half_population () (IF (EVENP *population_length*)
								       (nthcar (- *half_population_length* 1) *population*)
										Nil
								)
)

;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;|	create_new_generation (chromosome) -> This frunction takes the first half number of chromosomes in the   	 |;;
;;|								          populationbut and add to it the given chromosome and after that		 |;;
;;|										  fill the rest with random chromosomes	until arrive to the max length 	 |;;
;;| USES:  																										 |;;
;;| 	- pick_top_half_population ()								    										 |;;
;;| 	- generate_random_half_population ()								    								 |;;
;;|______________________________________________________________________________________________________________|;;


(defun create_new_generation (chromosome) (= *population_length* (LENGTH (EVAL (defparameter *population*  (APPEND (CONS chromosome (pick_top_half_population)) (generate_random_half_population))  )))))


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;|	exists_optimal_solution () -> T if exists a chromosome with fitness equal to 0.								 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- parent_selection_2  ()									    										 |;;
;;| 	- fitness_score (chromosome) 								    										 |;;
;;|______________________________________________________________________________________________________________|;;


(defun exists_optimal_solution () (
									let (( fitters (parent_selection_2) ))
									(IF (OR (= 0 (fitness_score (FIRST fitters))) (= 0 (fitness_score (SECOND fitters)) )) T NIL ) 
								  )
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;|	get_optimal_solution () -> Return the optimal solution.														 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- parent_selection_2  ()									    										 |;;
;;| 	- fitness_score (chromosome) 								    										 |;;
;;|______________________________________________________________________________________________________________|;;

(defun get_optimal_solution () (
								  let (( fitters  (parent_selection_2) ))
								  (IF  (= (fitness_score (FIRST fitters))  0 )  
									   (FIRST fitters)
									   (SECOND fitters)
								  )
								)
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;| algorithm_1 () -> This function implements the configuration of algorithm1 described in the paper            |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- mutation_1  ()									    												 |;;
;;| 	- crossover (chromosome_1 chromosome_2) 								    							 |;;
;;| 	- parent_selection_1 ()					 								    							 |;;
;;|______________________________________________________________________________________________________________|;;


(defun algorithm_1 () (mutation_1 (crossover (parent_selection_1) (parent_selection_1)))  )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;| algorithm_2 () -> This function implements the configuration of algorithm2 described in the paper            |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- mutation_2  ()									    												 |;;
;;| 	- crossover (chromosome_2 chromosome_2) 								    							 |;;
;;| 	- parent_selection_2 ()					 								    							 |;;
;;|______________________________________________________________________________________________________________|;;


(defun algorithm_2 (fitters_chromosomes) (mutation_2 (crossover (FIRST  fitters_chromosomes ) (SECOND fitters_chromosomes ) ) ) )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;|	choose_algorithm () -> As described in the paper in base of the fitness of the best chromosomes pair selects |;;
;;|						   an algorithm or another.														         |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- parent_selection_2 				     								    							 |;;
;;| 	- fitness_score (chromosome) 				     								    					 |;;
;;| 	- create_new_generation (chromosome)				     								    			 |;;
;;| 	- algorithm_1 ()				     									    							 |;;
;;| 	- algorithm_2 ()				     									    							 |;;
;;|______________________________________________________________________________________________________________|;;

(defun choose_algorithm () ( 
								let (( fitters  (parent_selection_2) ))
								(IF  (OR  (> ( fitness_score (FIRST fitters)) 4)  (>  (fitness_score (SECOND fitters)) 4) )     
								  (create_new_generation (algorithm_1)) 
								  (create_new_generation (algorithm_2 fitters))
							     )
						  )
)


;;.______________________________________________________________________________________________________________.;;
;;|											 																	 |;;
;;|											 																	 |;;
;;|											#	GENETIC ALGORITHM	#	 										 |;;
;;|											 																	 |;;
;;|______________________________________________________________________________________________________________|;;


(defun wisdom_of_artificial_crowds () 0 )


;; Funcion que lanza el algoritmo:

(defun start () (IF (EQ (exists_optimal_solution) T) 
					(get_optimal_solution)
					( IF (EQ (max_generation_reached) T)
						 (wisdom_of_artificial_crowds)
						 (IF (EQ (choose_algorithm) T) (start) Nil)
					)
				)
)

;;Funcion que lanza solo una iteracion del algoritmo y tras esta devuelve si existe o no una solucion

(defun start_one () (IF (EQ (exists_optimal_solution) T) 
					(get_optimal_solution)
					( IF (EQ (max_generation_reached) T)
						 (wisdom_of_artificial_crowds)
						 (IF (EQ (choose_algorithm) T) (exists_optimal_solution) NIL)
					)
				)
)
