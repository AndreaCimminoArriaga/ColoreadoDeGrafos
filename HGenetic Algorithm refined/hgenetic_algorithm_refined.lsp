;;Importing data from file 'data.lsp'
(load "data.lsp")


;;.______________________________________________________________________________________________________________.;;
;;|											 																	 |;;
;;| 		Initialice genetic algorithm parameters as described in the paper:									 |;;
;;|______________________________________________________________________________________________________________|;;


(defparameter *population_length* 2)
(defparameter *half_population_length* 1)
(defparameter *population* '())
(load "functions.lsp")
(defparameter *population* (generate_random_population))
(defparameter *generation* 0)
(defparameter *max_generations* 20000)

(defparameter *best_chromosomes* '() )


;;.______________________________________________________________________________________________________________.;;
;;|											 																	 |;;
;;|											 																	 |;;
;;|									#	GENETIC ALGORITHM MAIN FUNCTIONS	#	 								 |;;
;;|											 																	 |;;
;;|______________________________________________________________________________________________________________|;;



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
;;| 	- nthcar_2 (n list)											    										 |;;
;;|______________________________________________________________________________________________________________|;;


(defun pick_top_half_population () (IF (EVENP *population_length*)
								       (nthcar_2 (- *half_population_length* 1) *population*)
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


(defun create_new_generation (chromosome) (= *population_length* (LENGTH (EVAL (defparameter *population*  (APPEND (ACONS chromosome (fitness_score chromosome) (pick_top_half_population)) (generate_random_half_population)  )  )) )))


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
									  let (( best (EVAL ( defparameter *best_chromosomes* (parent_selection_2)))  ))
									  (IF (OR (= 0 (CDR (FIRST best))) (= 0 (CDR (SECOND best))))
											T
											Nil
									  )
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

(defun get_optimal_solution () (IF  (= (CDR (FIRST *best_chromosomes*))  0 )  
									(CAR (FIRST *best_chromosomes*))
									(CAR (SECOND *best_chromosomes*))
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


(defun algorithm_2 () (mutation_2 (crossover (CAR (FIRST  *best_chromosomes* )) (CAR (SECOND *best_chromosomes* )) ) ) )


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

(defun choose_algorithm () (IF  (OR  (> (CDR (FIRST *best_chromosomes*)) 4)  (>  (CDR (SECOND *best_chromosomes*)) 4) )     
								(create_new_generation (algorithm_1)) 
								(create_new_generation (algorithm_2))
							)
)

;;.______________________________________________________________________________________________________________.;;
;;|											 																	 |;;
;;|											 																	 |;;
;;|											#	wisdom_of_artificial_crowds	#	 								 |;;
;;|											 																	 |;;
;;|______________________________________________________________________________________________________________|;;




(defparameter *expert_chromosomes*  Nil)
(defparameter *color_array* Nil)
(defparameter *best_chromosome* Nil)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;|	inc_color_counter (color) -> Given a color increments the param *color_array* in the position of that color  |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- set_element (n elem list)					     								    					 |;;
;;|______________________________________________________________________________________________________________|;;



(defun inc_color_counter (color) ( IF (OR (<= color 0) (> color *k*)) 
									   Nil
									   (EVAL (defparameter *color_array* (set_element (- color 1) (+ 1 (NTH (- color 1) *color_array*)) *color_array* )     ))
									
								)
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;|	count_color (vertex chromosome) -> Given a vertex calculates how many times uses each color 			     |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- inc_color_counter (color)					     								    					 |;;
;;|______________________________________________________________________________________________________________|;;


(defun count_color (vertex chromosome) (inc_color_counter (vertex_color vertex chromosome))  )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;|	 most_used_color (vertex) -> Returns the array with the count of each color in base of how many times are    |;;
;;|								 used			 																 |;;
;;| USES:  																										 |;;
;;| 	- inc_color_counter (color)					     								    					 |;;
;;|______________________________________________________________________________________________________________|;;


(defun most_used_color_aux (vertex n) ( IF (= n  *half_population_length*)
									*color_array*
									(IF (EQ Nil (count_color vertex (CAR (NTH n *expert_chromosomes* ))  )  )
										Nil
										(most_used_color_aux vertex (+ n 1))	
									)
								))


(defun most_used_color (vertex) ( 
									let ((array_color  (most_used_color_aux vertex 0)))
									(IF (< 0 (LENGTH (EVAL (defparameter *color_array* (MAKE-LIST *k* :INITIAL-ELEMENT 0)  )))  )
										array_color
										Nil
									)
								) 
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;|	get_most_used_color (vertex) () -> Given a vertex returns the most used color with that vertex    		     |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- most_used_color (vertex)					     								    					 |;;
;;|______________________________________________________________________________________________________________|;;


(defun get_most_used_color_aux (counter counter_max value array_color) (IF (= counter (LENGTH array_color))
																		 counter_max
																		 (IF (> (NTH counter array_color) value)  
														  				 	 ( get_most_used_color_aux (+ 1 counter) counter (NTH counter array_color) array_color )
														  	  			 	 ( get_most_used_color_aux (+ 1 counter) counter_max value array_color)
													   		 		 	 )
														 			 ) 
)


(defun get_most_used_color (vertex) ( 
										let ((array_color (most_used_color vertex) ))
										(+ 1 (get_most_used_color_aux 0 0 0 array_color))
									
) )

;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;|	wisdom_of_artificial_crowds () -> As described in the paper this function it's called when the maximum       |;;
;;|						   			  generation ir reached and the algorithm has no found a solution.	         |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- set_element (n elem list)					     								    					 |;;
;;| 	- fitness_vertex_score (vertex chromosome) 							     								 |;;
;;| 	- get_most_used_color (vertex)			     								  							 |;;
;;|______________________________________________________________________________________________________________|;;


(defun wisdom_of_artificial_crowds_aux ( vertex ) (IF (= vertex *v*)
													  T
													  ( IF (= (fitness_vertex_score vertex *best_chromosome*) 0) 
													  	(wisdom_of_artificial_crowds_aux (+ 1 vertex))
													  	(IF (< 0 (LENGTH(EVAL(defparameter *best_chromosome* (set_element vertex (get_most_used_color vertex) *best_chromosome*) ))))
															(wisdom_of_artificial_crowds_aux (+ 1 vertex))
															Nil
													  	)
													  )
												  )
)

(defun apply_wisdom_of_artificial_crowds_aux (n) (IF (= n *v*)
													 T
													 (IF (EQ T (wisdom_of_artificial_crowds_aux n) )
													 	 (apply_wisdom_of_artificial_crowds_aux (+ 1 n))
														 Nil
													 )
												  )
)

(defun apply_wisdom_of_artificial_crowds  ()  (apply_wisdom_of_artificial_crowds_aux 0 ))


(defun wisdom_of_artificial_crowds () (
										 let (
										 		( best  (EVAL (defparameter *best_chromosome* (fitter (FIRST *best_chromosomes*) (SECOND *best_chromosomes*)))) )
										 	   	( best_half    (EVAL (defparameter *expert_chromosomes* (best_half_population)))  )
												(array_color (EVAL (defparameter *color_array* (MAKE-LIST *k* :INITIAL-ELEMENT 0)  )))
										 	 )
										  (IF (EQ T (apply_wisdom_of_artificial_crowds))
										  	   *best_chromosome*
											   Nil
										   )	
									  ) 
)


;;.______________________________________________________________________________________________________________.;;
;;|											 																	 |;;
;;|											 																	 |;;
;;|											#	GENETIC ALGORITHM	#	 										 |;;
;;|											 																	 |;;
;;|______________________________________________________________________________________________________________|;;



(defun start () (IF (EQ (exists_optimal_solution) T) 
					(get_optimal_solution)
					( IF (EQ (max_generation_reached) T)
						 (wisdom_of_artificial_crowds)
						 (IF (EQ (choose_algorithm) T) (start) Nil)
					)
				)
)

(defun start_one () (IF (EQ (exists_optimal_solution) T) 
					(get_optimal_solution)
					( IF (EQ (max_generation_reached) T)
						 (wisdom_of_artificial_crowds)
						 (IF (EQ (choose_algorithm) T) (exists_optimal_solution) NIL)
					)
				)
)


