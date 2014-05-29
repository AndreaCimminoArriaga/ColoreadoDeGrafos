;;|																												 |;;
;;|						##############################################################							 |;;
;;|						#####				Main and auxiliar functions    	    ######							 |;;
;;|						##############################################################						     |;;
;;|																											     |;;
;;|																												 |;;

;;|																												 |;;
;;| This file contains all the functions required by the algorithm, i write a little description of each one     |;;
;;| and the tipe (main or auxiliar). Normaly an auxiliar function is used in the next main function at any rate  |;;
;;| in the description of the function there are indicate the dependences.										 |;;
;;|																												 |;;
;;|	-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -|;;
;;|																												 |;;
;;| Funtions in the file:																						 |;;
;;|																												 |;;
;;|		- nthcar (n list)									: AUXILIAR FUNCTION (line 71)						 |;;
;;|		- nthcar_2 (n list)									: AUXILIAR FUNCTION (line 82)						 |;;
;;|		- set_element (n elem list)							: AUXILIAR FUNCTION (line 102)				 		 |;;
;;|		- vertex_color (vertex chromosome)					: AUXILIAR FUNCTION (line 118)		 				 |;;
;;|		- colors (vertex_list chromosome)					: AUXILIAR FUNCTION (line 130)						 |;;
;;|		- neighbour (vertex)								: AUXILIAR FUNCTION (line 141)						 |;;
;;|		- neighbour_colors (vertex chromosome)				: AUXILIAR FUNCTION (line 156)						 |;;
;;|		- bad_edges (color color_list n)					: AUXILIAR FUNCTION (line 169)						 |;;
;;|		- fitness_vertex_score (vertex chromosome)			: AUXILIAR FUNCTION (line 190)						 |;;
;;|		- fitness_score (chromosome)						: AUXILIAR FUNCTION (line 210)						 |;;
;;|		- fitter (pair_1 pair_2)							: AUXILIAR FUNCTION (line 223)						 |;;
;;|		- generate_random_array (n list)					: AUXILIAR FUNCTION (line 244)						 |;;
;;|		- generate_random_chromosome ()						: AUXILIAR FUNCTION (line 265)						 |;;
;;|		- pair_chromosome_score	(chromosome list)			: AUXILIAR FUNCTION (line 278)						 |;;
;;|		- generate_random_pair (list)						: AUXILIAR FUNCTION (line 294)						 |;;
;;|		- generate_random_array_list (n list)				: AUXILIAR FUNCTION (line 309)						 |;;
;;|		- remove_pl (pair)									: AUXILIAR FUNCTION (line 337)						 |;;
;;|		- generate_random_population ()						: FUNCTION 			(line 356)						 |;;
;;|		- generate_random_half_population					: FUNCTION 			(line 375)						 |;;
;;|		- best_chromosome ()								: AUXILIAR FUNCTION (line 398)						 |;;
;;|		- best_chromosomes ()								: AUXILIAR FUNCTION (line 413)						 |;;
;;|		- best_half_population ()							: AUXILIAR FUNCTION (line 434)						 |;;
;;|		- parent_selection_1 ()								: FUNCTION 			(line 474)						 |;;
;;|		- parent_selection_2 ()								: FUNCTION 			(line 489)						 |;;
;;|		- crossover (chromosome_1 chromosome_2)				: FUNCTION 			(line 509)						 |;;
;;|		- valid_colors (color_list)							: AUXILIAR FUNCTION (line 531)						 |;;
;;|		- pick_randomly (n list)							: AUXILIAR FUNCTION (line 543)						 |;;
;;|		- select_color (n chromosome)						: AUXILIAR FUNCTION (line 573)						 |;;
;;|		- mutation_1 (chromosome)							: FUNCTION 			(line 604)						 |;;
;;|		- color_random ()									: AUXILIAR FUNCTION (line 616)						 |;;
;;|		- mutation_2 (chromosome)							: FUNCTION 			(line 647)						 |;;
;;|																												 |;;



;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| nthcar (n list) -> This function returns the list from the elem 0 to the elem n-1, below there is a 		 |;;
;;|						a modification of this function that returns up to the n elem.							 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun nthcar_aux (n m list list_res) (IF (= m n)
										   (REVERSE list_res)
										   (IF (= (LENGTH list) 0)
										   		(REVERSE list_res)
												(nthcar_aux n (+ m 1) (CDR list)  (CONS (FIRST list) list_res)  )
											)
									   ) 
)

(defun nthcar (n list) (nthcar_aux n 0 list '()) )

(defun nthcar_aux_2 (n m list list_res) (IF (= m n)
										   (REVERSE (CONS(FIRST list) list_res))
										   (IF (= (LENGTH list) 0)
										   		(REVERSE list_res)
												(nthcar_aux_2 n (+ m 1) (CDR list)  (CONS (FIRST list) list_res)  )
											)
									   ) 
)

(defun nthcar_2 (n list) (nthcar_aux_2 n 0 list '()) )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;|	AUXILIAR FUNCTION: set_element (n elem list) -> This function take a list and a position from that list an   |;;
;;|													changes the elem in that position with the given by the user.|;;  
;;|													If the position is greather than  the lenght of the list the |;;
;;|													function will return the original list.						 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun set_element_aux (n m elem list list_res) ( IF (= n m)
													( APPEND (REVERSE (CONS elem list_res )) (NTHCDR (+ n 1) list) )
													(set_element_aux n (+ 1 m) elem list (CONS (NTH m list) list_res)  )
												) 
)


(defun set_element (n elem list) (IF (= n (LENGTH list) )
									 list
									 (set_element_aux n 0 elem list  '() )	 
								  ) 
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| vertex_color (vertex chromosome) -> Return the color of the vertex using a given chromosome.				 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun vertex_color (vertex chromosome) (NTH vertex chromosome))

;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| colors (vertex_list chromosome ) -> This function takes a list of vertex and calculate the color of each 	 |;;
;;|										vertex using a given chromosome and return a list with the colors of	 |;;
;;|										the vertexes.															 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;

(defun colors (vertex_list chromosome) (MAPCAR #'* vertex_list chromosome) )

;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| neighbour (vertex) -> Return the list of neighbours vertexes of a vertex given. 							 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun neighbour (vertex) (NTH vertex *matrix_adj*) )

;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| neighbour_colors (vertex chromosome) -> This functions return a list with the colors of his neighbour 		 |;;
;;|											vertex using a given chromosome.									 |;;
;;|																												 |;;
;;| USES:  																										 |;;
;;| 	- colors (vertex_list chromosome)																		 |;;
;;|		- neighbour (vertex)							    													 |;;
;;|______________________________________________________________________________________________________________|;;


(defun neighbour_colors (vertex chromosome) ( colors (EVAL (neighbour vertex)) chromosome)  )

;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| bad_edges (color color_list) -> Return the number of times that the given color appears in the list.		 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun eq_color (c1 c2) (IF (= c1 c2) 1 0 ) )

(defun bad_edges (color color_list n) (IF (= (LENGTH color_list) 0)
										  n
										  (bad_edges color (CDR color_list) (+ n (eq_color color (FIRST color_list)))  )
										  ) 
)
				
;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;						    
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| fitness_vertex_score (vertex chromosome) -> calculate the fitness score as described in the paper  			 |;;
;;|											    'Generic Algorithm Applied to the Graph Coloring Problem' for a	 |;;
;;|												given vertex.													 |;;
;;|																												 |;;
;;| USES:  																										 |;;
;;| 	- bad_edges (color color_list n)				    													 |;;
;;| 	- vertex_color (vertex chromosome)																		 |;;
;;| 	- neighbour_colors (vertex chromosome)																	 |;;
;;|______________________________________________________________________________________________________________|;;


(defun fitness_vertex_score (vertex chromosome) ( bad_edges (vertex_color vertex chromosome) (neighbour_colors vertex chromosome) 0) )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| fitness_score ( chromosome ) -> Calculate the fitness score as described in the paper 'Generic Algorithm     |;;
;;|									Applied to the Graph Coloring Problem'										 |;;
;;|																												 |;;
;;| USES:  																										 |;;
;;| 	- fitness_vertex_score (vertex chromosome)		    													 |;;
;;|______________________________________________________________________________________________________________|;;

(defun fitness_score_aux (chromosome n fitness) (IF (= n *v*) 
													fitness
													(fitness_score_aux chromosome (+ n 1)  (+ fitness (fitness_vertex_score n chromosome)) )
													)
)

(defun fitness_score (chromosome) (fitness_score_aux chromosome 0 0))


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| fitter (pair_1 pair_2) -> Returns the fitter chromosome. Namely, the chromosome with  less score			 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;



(defun fitter (pair_1 pair_2) ( IF (EQ (CDR pair_1) NIL)
									   (CAR pair_2)	
									   (IF (EQ (CDR pair_2) NIL)
									   	   (CAR pair_1)	
									   		(IF (< (CDR pair_1) (CDR pair_2) )
												(CAR pair_1)
												(CAR pair_2)
											)
										)
							 )
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;|	AUXILIAR FUNCTION: 																							 |;;
;;|														  														 |;;
;;| generate_random_array (n list) -> Generates an array filled with random numbers in the range ok *k* and 	 |;;
;;|									  which length is: ( *v* - n ).												 |;;
;;|______________________________________________________________________________________________________________|;;

(defun generate_random_array (n list) 
									(IF (= n *v*)
										list
										(  generate_random_array  (+ n 1) (cons (+ 1 (random *k*)) list ) )
									)
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;|	generate_random_chromosome () -> Generates in a random way a chromosome using the params defined in the data |;;
;;|									 file. Namely, an array a length of *v* and filled of collors in the range   |;;
;;|									 of *k* choosed randomly.													 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- generate_random_array (n list) 										    							 |;;
;;|______________________________________________________________________________________________________________|;;


(defun generate_random_chromosome () (generate_random_array 0 '() )  )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| pair_chromosome_score (chromosome list) -> Generates a pair chromosome and his score						 |;;			 ;;|										 																	  |;;
;;| USES:  																										 |;;
;;| 	- fitness_score (chromosome)  										    								 |;;
;;|______________________________________________________________________________________________________________|;;


(defun pair_chromosome_score (chromosome list) (ACONS chromosome (fitness_score chromosome)  list))


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| generate_random_pair (list) -> Generates a random chromosome and return a pair with his score and 			 |;;	
;;|								   the chromosome.	 													         |;;
;;|										 					   												     |;;
;;| USES:  																										 |;;
;;| 	- pair_chromosome_score (chromosome list) 							    								 |;;
;;|		- generate_random_chromosome ()								 				   						     |;;
;;|______________________________________________________________________________________________________________|;;


(defun generate_random_pair (list) (pair_chromosome_score (generate_random_chromosome) list)  )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION:  																						 |;;		
;;|										 																		 |;;
;;| generate_random_array_list (n list) -> Generates a list of array of random numbers in the range of *k* and   |;;
;;|											which size is: ( *v* - n ).											 |;;
;;|										 					   												     |;;
;;| USES:  																										 |;;
;;| 	- generate_random_pair (list)		   								    								 |;;
;;|______________________________________________________________________________________________________________|;;


(defun generate_random_array_list (n list) 
									(IF (= n *population_length*)
										list
										( generate_random_array_list  (+ n 1) (generate_random_pair list) )
									)
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIR FUNCTION: 																							 |;;
;;|																												 |;;
;;| remove_pl (pair) -> Remove from the population the pair given by argument and returns the pair				 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- nthcar (n list)							    														 |;;
;;|______________________________________________________________________________________________________________|;;


(defun remove_pl_aux (pair  n list) (IF (>= n (LENGTH list) )
										 list
										 (IF (EQ pair (NTH n list))
										 	  (remove_pl_aux pair (LENGTH list) (APPEND (nthcar n list) (nthcdr (+ n 1) list)) )
											  (remove_pl_aux pair (+ n 1) list)
										 )
									  )
)

(defun remove_pl (pair) (IF (<   (LENGTH (EVAL (defparameter *population* (remove_pl_aux pair 0 *population*))))  *population_length*  ) 
							pair
							Nil
						)
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|										 																		 |;;
;;| generate_random_population () -> Generates a pair list of random chromosomes, with their scores,which size   |;;
;;|									 is *population_length*.													 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- generate_random_array_list (n list) 										    						 |;;
;;|______________________________________________________________________________________________________________|;;


(defun generate_random_population () (
	 									let (( list  (generate_random_array_list 0 '() )   ))
										list
									  )
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;| generate_random_half_population () -> Generates a list of random chromosomes which size is the half of the   |;;
;;|										  param *population_length*.											 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- generate_random_array_list (n list) 										    						 |;;
;;|______________________________________________________________________________________________________________|;;


(defun generate_random_half_population () (
	 										let (( list  (generate_random_array_list (+ *half_population_length* 1) '() )   ))
											list 
										  )
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| best_chromosome () -> Returns the best chromosome from the population, but erases the chromosome from the    |;;
;;|						  param *population*.											 						 |;;
;;|											 																	 |;;
;;|	USES:																										 |;;
;;|		- remove_pl (pair)										 												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun best_chromosome_aux (n) ( IF (EQ (RASSOC n *population*) Nil)
									 (best_chromosome_aux (+ n 1)  )
									 (remove_pl (RASSOC n *population*) )
								) 
)

(defun best_chromosome () (best_chromosome_aux 0) )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| best_chromosomes () -> Returns the two best chromosomes from the population, but don't erases them  from	 |;;
;;|						   the param *population*.											 				 	 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- best_chromosome () 														    						 |;;
;;|______________________________________________________________________________________________________________|;;


(defun best_chromosomes ()  (
								let ((the_bests (LIST (best_chromosome) (best_chromosome)) ))
								(IF (< 0 (LENGTH  (EVAL (defparameter *population* (PAIRLIS (LIST (CAR (FIRST the_bests)) (CAR (SECOND the_bests))) (LIST (CDR (FIRST the_bests)) (CDR (SECOND the_bests)))   *population* ) )   ) )  )   
									the_bests
									Nil
								)
							) 
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| best_half_population () -> Returns the best chromosomes from the half of the population						 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- remove_pl (pair) 															    						 |;;
;;|______________________________________________________________________________________________________________|;;


(defun best_half_population_aux (n chromosomes) (
												  let (( pair (RASSOC n *population*) ))
												( IF (EQ pair Nil)
									 				 (best_half_population_aux (+ n 1) chromosomes )
									 		   		 (IF (= *half_population_length* (LENGTH  chromosomes))
													      chromosomes
														  (best_half_population_aux n (ACONS (CAR (remove_pl pair)) (CDR pair)  chromosomes ) )
													  )
												)
												) 
)

(defun best_half_population ()  (
									let ((  best_half  (best_half_population_aux 0 '()) ))
									(IF (< 0 (LENGTH (EVAL(defparameter *population* (APPEND  best_half *population* )))))
										best_half
										Nil
									)
								)   
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION:         																							 |;;
;;|																												 |;;
;;| parent_selection_1 () -> Return one chromosomes using the criteria described in the paper 'Generic Algorithm |;;
;;|							 Applied to the Graph Coloring Problem', in the papper isdescribed that the          |;;
;;|							 chromosomes returned by this function are two, but for programming reasons this     |;;
;;| 						 function will return just one and if in future we need two we call the function     |;;
;;|							 twice.																				 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- fitter (chromosome1 chromosome2) 															    		 |;;
;;|______________________________________________________________________________________________________________|;;



(defun parent_selection_1_aux () (fitter (NTH (RANDOM *population_length*) *population*) (NTH (RANDOM *population_length*) *population*) ))

(defun parent_selection_1 ()  (parent_selection_1_aux) )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION:         																							 |;;
;;|																												 |;;
;;| parent_selection_2 () -> Return one chromosomes using the criteria described in the paper 'Generic Algorithm |;;
;;|							 Applied to the Graph Coloring Problem'. 											 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- fitters_chromosomes ()		 															    		 |;;
;;|______________________________________________________________________________________________________________|;;


(defun parent_selection_2 () (best_chromosomes))


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;| crossover (chromosome1 chromosome2 crosspoint) -> This function implements the function with the same name   |;;
;;|													  described in the paper. When it's call the crosspoint must |;;
;;|													  be a random number in the range of a chromosome. 			 |;;
;;|													  The function has been adapted to be able to get that number|;; 
;;|													  from the function RANDOM.									 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- nthcar_2 (n list)				 															    		 |;;
;;|______________________________________________________________________________________________________________|;;


(defun crossover_aux (chromosome1 chromosome2 crosspoint) (APPEND (nthcar_2 crosspoint chromosome1 ) (NTHCDR (+ 1 crosspoint) chromosome2)) )

(defun crossover (chromosome1 chromosome2) (crossover_aux chromosome1 chromosome2 (RANDOM (LENGTH chromosome1))))



;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;|	AUXILIAR FUNCTION:																							 |;;
;;|																												 |;;
;;| valid_colors (color_list) ->  By a color list given return the colors non used.  							 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun valid_colors_aux (color_list k list) (IF (/= k 0)
												  (IF (EQ (MEMBER k color_list) NIL)
												  	   (valid_colors_aux color_list (- k 1) (CONS k list) )
													   (valid_colors_aux color_list (- k 1) list )
												   )
												   list
										  ) 
)

(defun valid_colors (color_list) ( valid_colors_aux color_list *k* '() ) )


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;|	AUXILIAR FUNCTION:																							 |;;
;;|																												 |;;
;;| pick-randomly (n l) ->  This function pick up in a random way n elements from the list l.  					 |;;
;;|																												 |;;
;;|______________________________________________________________________________________________________________|;;


(defun pick-randomly (n l) (if (= n 0)
   							   '()
    						   (IF (<= (LENGTH l) 0)
							   	   '()
	    						   (let ((pos-select (random (length l))))
	      						 		(cons (nth pos-select l)
		    								(pick-randomly (- n 1)
					   					 		(append (subseq l 0 pos-select)
						   					 		(subseq l (1+ pos-select))))))	
							   )

							)
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;|	AUXILIAR FUNCTION:																							 |;;
;;|																												 |;;
;;| select_color (n chromosome)  ->  Given a vertex the function return a 'valid color' given the neighbour		 |;;
;;|									 color and the vertex color. If not exists a valid color returns the vertex  |;;
;;|									 same color.																 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- valid_colors (color_list)  			- pick_randomly (n list)	 															    		 |;;
;;|		- vertex_color (vertex chromosome)									 									 |;;
;;|______________________________________________________________________________________________________________|;;



(defun select_color (n chromosome)  (IF (EQ Nil (set 'final_color (FIRST (pick-randomly 1 (valid_colors (neighbour_colors n chromosome) ))))   )
										(vertex_color n chromosome)
										(EVAL 'final_color)
						)
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;| mutation_1  (chromosome n) -> The function implements the steps described in the paper for the function      |;;
;;|									 mutation1.																	 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- fitness_vertex_score (vertex chromosome)																 |;;
;;| 	- set_element (n elem list)																				 |;;
;;|		- select_color (n chromosome)											 								 |;;
;;|______________________________________________________________________________________________________________|;;


(defun mutation_1_aux (chromosome n) (IF (>= n *v*)
										 chromosome
										 (IF (= (fitness_vertex_score n  chromosome) 0 ) 
									 		 (mutation_1_aux chromosome (+ n 1) )
										 	 (mutation_1_aux (set_element n (select_color n chromosome) chromosome ) (+ n  1)    )
										 )
									 )							
)


(defun mutation_1 (chromosome) (mutation_1_aux chromosome 0))

;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION: 																							 |;;
;;|																												 |;;
;;| color_random   -> The function return a random color													     |;;
;;|									 																			 |;;
;;|											 																	 |;;
;;|______________________________________________________________________________________________________________|;;


(defun color_random () (
						LET ( (color (random *k*)))
							(IF (= 0 color)  
								(color_random)
								color
						     )
					    )
)

;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| FUNCTION: 																									 |;;
;;|																												 |;;
;;| mutation_2  (chromosome n) -> The function implements the steps described in the paper for the function      |;;
;;|									 mutation2.																	 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- fitness_vertex_score (vertex chromosome)																 |;;
;;| 	- set_element (n elem list)																				 |;;
;;|		- color_random ()														 								 |;;
;;|______________________________________________________________________________________________________________|;;

(defun mutation_2_aux (chromosome n) (IF (>= n (LENGTH chromosome))
										 chromosome
										 (IF (= (fitness_vertex_score (NTH n chromosome)  chromosome) 0 ) 
									 		 (mutation_2_aux chromosome (+ n 1) )
										 	 (mutation_2_aux (set_element n (color_random) chromosome ) (+ n 1))
										 )
									  )
)

(defun mutation_2 (chromosome) (mutation_2_aux chromosome 0))









