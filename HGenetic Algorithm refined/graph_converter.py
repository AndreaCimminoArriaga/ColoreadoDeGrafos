import sys

x = 0
y = 0

#Check if the args are correct
if len(sys.argv) == 3:
    
    file_name = sys.argv[1]
    colors = sys.argv[2]
    vertexes = "0"
    matrix = []
   
    
    #Creating the output file
    data_file=open('data.lsp','w')
    #Opening the txt file
    file = open(file_name)
    
    line = file.readline()
    while line!="":
        #checking if the line is the line with the number of vertexes
        if "edge" in line:
            data = line.split(" ")
            #We create the adjacent matrix using the number of vertex and filling it wiht 0's
            
            vertexes = data[2]
            matrix = [[0] * int(data[2]) for i in range(int(data[2]))]
           
        
        if (not "c" in line) and (not "p" in line):
            data = line.split(" ")
            #rescuting the row and the column
            row = int(data[1])
            column = int(data[2])
            
            #setting that position to 1
            matrix[row-1][column-1] = 1
         
        line=file.readline()
    
    file.close()
    
    #Writing the data in the file
    line1 = "(defparameter *v* "+vertexes+") \n"
    data_file.write(line1)
    
    line2 = "(defparameter *k* "+colors+" ) \n"
    data_file.write(line2)
    
    
    matrix_string = ""
    for row in matrix:
        matrix_string = matrix_string + str(row).replace(',','').replace('[',"'(").replace(']',")")+" "
    
    line3 = "(defparameter *matrix_adj*  '( "+matrix_string+" )   ) \n"
    data_file.write(line3)
   
  
    data_file.close()
else:
    print "ERROR -> Please tipe: ./ggc_util.py [file_name] [number_of_colors] \n"