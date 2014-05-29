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
;;|		- nthcar (n list)									: AUXILIAR FUNCTION (line 58)						 |;;
;;|		- nthcar_2 (n list)									: AUXILIAR FUNCTION (line 69)						 |;;
;;|		- vertex_color (vertex chromosome)					: AUXILIAR FUNCTION (line 81)		 				 |;;
;;|		- colors (vertex_list chromosome)					: AUXILIAR FUNCTION (line 95)						 |;;
;;|		- neighbour (vertex)								: AUXILIAR FUNCTION (line 107)						 |;;
;;|		- neighbour_colors (vertex chromosome)				: AUXILIAR FUNCTION (line 123)						 |;;
;;|		- bad_edges (color color_list n)					: AUXILIAR FUNCTION (line 137)						 |;;
;;|		- fitness_vertex_score (vertex chromosome)			: AUXILIAR FUNCTION (line 159)						 |;;
;;|		- fitness_score (chromosome)						: AUXILIAR FUNCTION (line 180)						 |;;
;;|		- generate_random_array (n list)					: AUXILIAR FUNCTION (line 192)						 |;;
;;|		- generate_random_chromosome ()						: AUXILIAR FUNCTION (line 213)						 |;;
;;|		- pair_chromosome_score	(chromosome list)			: AUXILIAR FUNCTION (line 226)						 |;;
;;|		- generate_random_pair (list)						: AUXILIAR FUNCTION (line 242)						 |;;
;;|		- generate_random_array_list (n list)				: AUXILIAR FUNCTION (line 254)						 |;;
;;|		- set_element (n elem list)							: AUXILIAR FUNCTION (line 279)						 |;;
;;|		- pick_randomly (n list)							: AUXILIAR FUNCTION (line 295)						 |;;
;;|		- valid_colors (color_list)							: AUXILIAR FUNCTION (line 328)						 |;;
;;|		- select_color (n chromosome)						: AUXILIAR FUNCTION (line 346)						 |;;
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
;;|______________________________________________________________________________________________________________|;;


(defun generate_random_array_list (n list top) 
									(IF (= n top)
										list
										( generate_random_array_list  (+ n 1) (cons (generate_random_chromosome) list ) top)
									)
)


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
;;| select_color (n chromosome)  ->  Given a vertex the function return a 'valid color' given the neighbour		 |;;
;;|									 color and the vertex color. If not exists a valid color returns the vertex  |;;
;;|									 same color.																 |;;
;;|											 																	 |;;
;;| USES:  																										 |;;
;;| 	- valid_colors (color_list)  	 															    		 |;;
;;|		- vertex_color (vertex chromosome)									 									 |;;
;;|		- pick_randomly (n list)									 											 |;;
;;|______________________________________________________________________________________________________________|;;


(defun select_color (n chromosome)  (IF (EQ Nil (set 'final_color (FIRST (pick-randomly 1 (valid_colors (neighbour_colors n chromosome) ))))   )
										(vertex_color n chromosome)
										(EVAL 'final_color)
						)
)


;;.______________________________________________________________________________________________________________.;;
;;|																												 |;;
;;| AUXILIAR FUNCTION:  																						 |;;		
;;|										 																		 |;;
;;| generate_random_array_list_pairs (n list top) -> Generates a list of array of random numbers in the range    |;;
;;|												of *k* and which size is: ( *v* - n ). And their fitness score.	 |;;
;;|										 					   												     |;;
;;| USES:  																										 |;;
;;| 	- generate_random_pair (list)		   								    								 |;;
;;|______________________________________________________________________________________________________________|;;


(defun generate_random_array_list_pairs (n list top) 
									(IF (= n top)
										list
										( generate_random_array_list_pairs  (+ n 1) (generate_random_pair list) top )
									)
)





