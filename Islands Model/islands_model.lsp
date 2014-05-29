;;Importing data from file 'data.lsp'
(load "data.lsp")

;;Importing functions from file 'functions.lsp'
(load "functions.lsp")


;;.______________________________________________________________________________________________________________.;;
;;|											 																	 |;;
;;| 		Initialice genetic algorithm and island model parameters as described in the paper:					 |;;
;;|______________________________________________________________________________________________________________|;;


(defparameter *island_1* '())
(defparameter *island_2* '())
(defparameter *island_3* '())
(defparameter *island_4* '())
(defparameter *generation* 0)
(defparameter *max_generations* 20000)
(defparameter *emigrate_each_n_generations* 4)

;;.______________________________________________________________________________________________________________.;;
;;|											 																	 |;;
;;|											 																	 |;;
;;|								#	ISLAND MODEL AUXILIAR AND MAIN FUNCTIONS	#	 							 |;;
;;|											 																	 |;;
;;|______________________________________________________________________________________________________________|;;



;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;						    
;;| FUNCTION: 																							 		 |;;
;;|																												 |;;
;;| distribute_population (population) -> If the length of the given param population is not divisible by five   |;;
;;|										  fills the given param population with random pair chromosome and his	 |;;
;;|										  fitness score when reach a number of elems that is divisible then		 |;;
;;|										  distribute the population by the islands params.						 |;;
;;| USES:  																										 |;;
;;| 	- nthcar (n list)				    																	 |;;
;;| 	- generate_random_pair (list)				    														 |;;
;;|______________________________________________________________________________________________________________|;;


(defun distribute_population_aux (population) (
												let (
														(island1 (EVAL (defparameter *island_1* ( nthcar (/ (LENGTH population) 4) population) ) ))
														(island2 (EVAL (defparameter *island_2* ( nthcdr (/ (LENGTH population) 4) (nthcar (* (/ (LENGTH population) 4) 2) population)) ) ))
														(island3 (EVAL (defparameter *island_3* ( nthcdr (* (/ (LENGTH population) 4) 2) (nthcar (* (/ (LENGTH population) 4) 3) population)) )))
														(island4 (EVAL (defparameter *island_4* ( nthcdr (* (/ (LENGTH population) 4) 3) (nthcar (* (/ (LENGTH population) 4) 4) population)) )))
														
													)
												(AND (= (LENGTH island1) (LENGTH island2) (LENGTH island3) (LENGTH island4) )  )
												)
) 




(defun distribute_population (population) ( IF (= (MOD (LENGTH population) 4) 0)
							   (distribute_population_aux population)
							   (distribute_population (generate_random_pair population) )
	)		
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;						    
;;| FUNCTION: 																							 		 |;;
;;|																												 |;;
;;| transform_population (population) -> Takes the population and create an A-list with each elem of the given   |;;
;;|										 list and the fitness_score of each chromosome of the given list.	     |;;
;;|																												 |;;						    
;;| USES:  																										 |;;
;;| 	- fitness_score (chromorome)				    														 |;;
;;|______________________________________________________________________________________________________________|;;
 

(defun transform_population_aux (population result n) ( let (( elem (NTH n population)))
													(IF (= (LENGTH population) (LENGTH result))
													   result
													   (transform_population_aux population (APPEND result (ACONS elem (fitness_score elem) '())) (+ n 1))
													)
												  )   
)

(defun transform_population (population) (transform_population_aux population '() 0))


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;						    
;;| AUXILIAR FUNCTION: 																					 		 |;;
;;|																												 |;;
;;| add_to_island (list) -> Takes the list given by param and add it to the corresponding island.			     |;;
;;|																												 |;;						    
;;|______________________________________________________________________________________________________________|;;


(defun add_to_island_1 (list) (< 0 (LENGTH (EVAL (defparameter *island_1* (APPEND list *island_1*))))))
(defun add_to_island_2 (list) (< 0 (LENGTH (EVAL (defparameter *island_2* (APPEND list *island_2*))))))
(defun add_to_island_3 (list) (< 0 (LENGTH (EVAL (defparameter *island_3* (APPEND list *island_3*))))))
(defun add_to_island_4 (list) (< 0 (LENGTH (EVAL (defparameter *island_4* (APPEND list *island_4*))))))


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;						    
;;| AUXILIAR FUNCTION: 																					 		 |;;
;;|																												 |;;
;;| take_best3_island (list) -> Takes the best three chromosomes from the corresponding island and erase from    |;;
;;|								there.																			 |;;						    
;;| USES:  																										 |;;
;;| 	- nthcar (n list)				    																	 |;;
;;|______________________________________________________________________________________________________________|;;


(defun take_best3_island1 () (let (( ordered_island (sort *island_1* #'< :key #'cdr) ))
							  (IF (<= 0 (LENGTH (EVAL (defparameter *island_1* (nthcdr 3 *island_1*) )))) 	
								(nthcar 3  ordered_island)
								Nil
							  )
							 )
)

(defun take_best3_island2 () (let (( ordered_island (sort *island_2* #'< :key #'cdr) ))
							  (IF (<= 0 (LENGTH (EVAL (defparameter *island_2* (nthcdr 3 *island_2*) )))) 	
								(nthcar 3  ordered_island)
								Nil
							  )
							 )
)

(defun take_best3_island3 () (let (( ordered_island (sort *island_3* #'< :key #'cdr) ))
							  (IF (<= 0 (LENGTH (EVAL (defparameter *island_3* (nthcdr 3 *island_3*) )))) 	
								(nthcar 3  ordered_island)
								Nil
							  )
							 )
)

(defun take_best3_island4 () (let (( ordered_island (sort *island_4* #'< :key #'cdr) ))
							  (IF (<= 0 (LENGTH (EVAL (defparameter *island_4* (nthcdr 3 *island_4*) )))) 	
								(nthcar 3  ordered_island)
								Nil
							  )
							 )
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;						    
;;| FUNCTION: 																							 		 |;;
;;|																												 |;;
;;| emigrate (population) -> This function take the best three chromosomes from each island and makes them 	     |;;
;;|							 emigrate like is described in the paper of this work.								 |;;
;;|																												 |;;
;;| USES:  																										 |;;
;;| 	- take_best3_island	()			    																	 |;;
;;| 	- add_to_island (list)						    														 |;;
;;|______________________________________________________________________________________________________________|;;


(defun emigrate () ( let (
							(best3_1  (take_best3_island1))
							(best3_2  (take_best3_island2))
							(best3_3  (take_best3_island3))
							(best3_4  (take_best3_island4))
							
						  )
						(AND 
							(add_to_island_1 (LIST (FIRST best3_3) (SECOND best3_4) (FIRST best3_2) ) )
							(add_to_island_2 (LIST (THIRD best3_1) (SECOND best3_3) (THIRD best3_4) ) )
							(add_to_island_3 (LIST (FIRST best3_1) (SECOND best3_2) (FIRST best3_4) ) )
							(add_to_island_4 (LIST (THIRD best3_3) (SECOND best3_1) (THIRD best3_2) ) )
							
						)  
						
					)
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;						    
;;| FUNCTION: 																							 		 |;;
;;|																												 |;;
;;| exists_solution_in_island () -> This function return a list with the solution that finds in each island or   |;;
;;|							 		Nil if they don't exists.													 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun exists_solution_in_island () (LIST (RASSOC 0 *island_1*) (RASSOC 0 *island_2*) (RASSOC 0 *island_3*) (RASSOC 0 *island_4*) )   )


;;.______________________________________________________________________________________________________________.;;
;;|											 																	 |;;
;;|											 																	 |;;
;;|									#	GENETIC ALGORITHM MAIN FUNCTIONS	#	 								 |;;
;;|											 																	 |;;
;;|______________________________________________________________________________________________________________|;;




;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;						    
;;| FUNCTION: 																							 		 |;;
;;|																												 |;;
;;| get_solution (list) -> This function return a solution if exists.										     |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun get_solution (list) (IF (EQ (FIRST list) NIL) 
								(IF (EQ (SECOND list) NIL)  
									(IF (EQ (THIRD list) NIL)       
										(IF (EQ (NTH 3 list) NIL)
											NIL
											(NTH 3 list)
										)
										(THIRD list)  
									) 
									(SECOND list) 
								)  
								(FIRST list)  
							)   
)


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
;;| FUNCTION:         																							 |;;
;;|																												 |;;
;;| parent_selection () -> Return the two best chromosomes in the given population.							     |;;
;;|											 																	 |;;
;;|______________________________________________________________________________________________________________|;;


(defun parent_selection (population) (nthcar 2 (sort population #'< :key #'cdr) ) )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;| crossover (chromosome1 chromosome2) -> This function cross two parents in a random way, like in the prior 	 |;;
;;|											work.																 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- nthcar_2 (n list)				 															    		 |;;
;;|______________________________________________________________________________________________________________|;;


(defun crossover_aux (chromosome1 chromosome2 crosspoint) (APPEND (nthcar_2 crosspoint chromosome1 ) (NTHCDR (+ 1 crosspoint) chromosome2)) )

(defun crossover (chromosome1 chromosome2) (crossover_aux chromosome1 chromosome2 (RANDOM (LENGTH chromosome1))))


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;| mutation  (chromosome n) -> The function implements the same steps as the mutation_1 in the prior work.      |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- fitness_vertex_score (vertex chromosome)																 |;;
;;| 	- set_element (n elem list)																				 |;;
;;|		- select_color (n chromosome)											 								 |;;
;;|______________________________________________________________________________________________________________|;;


(defun mutation_aux (chromosome n) (IF (>= n *v*)
										 chromosome
										 (IF (= (fitness_vertex_score n  chromosome) 0 ) 
									 		 (mutation_aux chromosome (+ n 1) )
										 	 (mutation_aux (set_element n (select_color n chromosome) chromosome ) (+ n  1)    )
										 )
									 )							
)


(defun mutation (chromosome) (mutation_aux chromosome 0))


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;| create_new_generation (chromosome population) -> This functions takes the new chromosome and add it to the   |;;
;;|								  					 population, ten order the population by fitness score and	 |;;
;;|											 		 erase the worst half and add a new random half.			 |;;
;;|																												 |;;
;;| USES:  																										 |;;
;;| 	- fitness_vertex_score (vertex chromosome)																 |;;
;;| 	- set_element (n elem list)																				 |;;
;;|		- select_color (n chromosome)											 								 |;;
;;|______________________________________________________________________________________________________________|;;


(defun create_new_generation_aux (chromosome population)  (set_element (- (LENGTH population) 1) (FIRST(ACONS chromosome (fitness_score chromosome) '() )) 
(sort population #'< :key #'cdr)) )

(defun create_new_generation (chromosome population)  (
														let (
															(best_half (nthcar (FLOOR (/ (LENGTH population) 2))  (sort population #'< :key #'cdr)))
															(tam (LENGTH population))
														)
														(IF (EQ (EVENP tam) T)     
															(APPEND (create_new_generation_aux chromosome best_half) (generate_random_array_list_pairs 0 '() (/ tam 2) ))
															(APPEND (create_new_generation_aux chromosome best_half) (generate_random_array_list_pairs 0 '() (+ 1 (FLOOR (/ tam 2)) )))
													    )
													  )  
)

(defun create_new_generation_2 (chromosome population) (create_new_generation_aux chromosome population))

;;.______________________________________________________________________________________________________________.;;
;;|											 																	 |;;
;;|											 																	 |;;
;;|									#	ISLAND MODEL & GENETIC ALGORITHM 	#									 |;;
;;|											 																	 |;;
;;|______________________________________________________________________________________________________________|;;

(defun genetic_algorithm (population) ( let (
												(chromosomes (parent_selection population))
											)
	 									(create_new_generation (mutation (crossover (CAR (FIRST  chromosomes )) (CAR (SECOND chromosomes )) ))  population)
									   )   
)

(defun genetic_algorithm_2 (population) ( let (
												(chromosomes (parent_selection population))
											)
	 									(create_new_generation_2 (mutation (crossover (CAR (FIRST  chromosomes )) (CAR (SECOND chromosomes )) ))  population)
									   )   
)


(defun lauch_genetics_algorithms () (let (
											( result_1   (LENGTH (EVAL (defparameter *island_1* (genetic_algorithm *island_1*)))) )
											( result_2   (LENGTH (EVAL (defparameter *island_2* (genetic_algorithm *island_2*)))) )
											( result_3   (LENGTH (EVAL (defparameter *island_3* (genetic_algorithm_2 *island_3*)))) )
											( result_4   (LENGTH (EVAL (defparameter *island_4* (genetic_algorithm_2 *island_4*)))) )
										  )
										  (IF (< 0 (+ result_1 result_2 result_3 result_4)) T Nil)
									) 
) 


(defun start ()  ( 
					let (( solution (get_solution (exists_solution_in_island)) ))  
				 
				 	(IF (NOT (EQ NIL solution))
						(CAR solution)
						(IF (EQ T (max_generation_reached) )
							0
							(IF (= 0 (MOD *generation* *emigrate_each_n_generations*))
								(IF (EQ (AND (emigrate) (lauch_genetics_algorithms)) T) (start) Nil )
								(IF (EQ (lauch_genetics_algorithms) T) (start) Nil)
							)
						)
				 	)
				)
)




(defun start_island_model (population) (
								  let ( 
								  		(correct_distributed (distribute_population(transform_population population))  )
									  )
								  (IF (EQ correct_distributed T) (start) NIL)
								)
)


(defparameter *population* (generate_random_array_list 0 '() 25))









