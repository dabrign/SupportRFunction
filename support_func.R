# get_vector function
# inputs:
# col_n --> whic col you want to import
# row_start --> at whic row you want to start
# row_stop --> at whic row you want to stop
# matrix_name --> name of the matrix that you want to use
# Output --> 
# choice of measures and whether the results are printed.

get_vector <- function(cols_n, row_start, row_stop, matrix_name){
#output<-matrix_name[row_start:row_stop,cols_n]
output<-vector('integer')
output<-matrix_name[,cols_n]
return(output)
}

# matrix_imported function
# this function import tab delimiter file where decimal separator is ","
# inputs:
# url --> path of the file to import
# Output --> matrix of the dataframe imported
# choice of measures and whether the results are printed.
matrix_imported <- function(url){
output <-as.matrix(read.table(url, dec=",", sep="\t"))
return(output)
}

# concatenated
# this function merge two vector
# inputs:
# first --> first vector
# second --> second vector
# Output --> contatenated vector
# choice of measures and whether the results are printed.
concatenated <- function(first, second){
output <-c(first,second)
return(output)
}

# concatenated
# this function merge two vector
# inputs:
# first --> first vector
# second --> second vector
# Output --> contatenated vector
# choice of measures and whether the results are printed.
mypmf_lines <- function(this_vector){
	GAP<-100-(this_vector*100)
	GAP[is.nan(GAP)] <- 0
	Vpmf<-vector()
	for(i in 1:100){ 
		Vpmf[i]<-sum(GAP>=(i-1) & GAP<i);
		}
	Vpmf<-Vpmf/sum(Vpmf)
	return(Vpmf)
}




Mvalues <- function(url){
this_time<-vector('numeric')
for(j in 1:150){
	url2<-"/Sol_pool.txt"
	this_url<-paste(url,toString(j), sep="")
	this_url<-paste(this_url,url2, sep="")
		if (file.exists(this_url)){
			this_data <-matrix_imported(this_url)
			this_values <- get_vector(6, 1, 150, this_data)
			this_values <- as.numeric(this_values)
			this_time<-concatenated(this_time,this_values)
		}
	}
return(this_time)
}
mypmf_lines_01_step <- function(this_vector){
	step_vector<-c(1:500)
	step_vector<-step_vector/100
	Vpmf<-vector()
	for(i in 1:500){ 
		Vpmf[i]<-sum(this_vector>=(step_vector[i-1]) & this_vector<step_vector[i]);
		}
	Vpmf<-Vpmf/sum(Vpmf)
	return(Vpmf)
}



