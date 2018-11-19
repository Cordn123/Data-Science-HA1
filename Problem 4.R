#Problem 4
rm(list=ls())

how_many_children <- function(vector){ #our function is depended on a given vector
  #creating the parameters that will be used throughout the whole solution
  #what this does is replicating a vector of zeros of a length of a given vector by a user
  num_children = rep(0, length(vector)) 
  #compressing the vector using rle so that it transforms to a convenient form to work with later on
  compressed = rle(vector) 
  #creating a vector, which is taking the difference between the number of elements of rle subgroup 1 and the length of a vector 
  v1 <- diff(c(compressed[[1]][1], length(vector)))
  
  #A case of five zeroes in a row:
  #For rle subgroups with 5 zeroes in a row 
  if (length(which(compressed[[1]] >= 5)) != 0) {
    #ascertaining the birth of a new child
    detect_new_child = which(compressed[[1]]>=5)
    #creating a cumulasted sum to properly add new child in a system
    compressed_sum = cumsum(compressed[[1]])
    # 
    if (compressed[[2]][1]==0){
      #this is the core of the accountancy of the children 
      #we rewrite num_of_child using previous record and adding a vector zeros and ones of a length of calculated vector of difference 
      num_children = num_children + c(rep(0,compressed[[1]][1]), rep(1,v1[1]))
    } else {
      num_children = num_children + rep(1, length(vector))  
    } 
    #here we analyzing where exactly to place a child
    for (i in detect_new_child){
      v2 <- diff(c(compressed_sum[[i]], length(vector)))
      if(compressed[[2]][i] == 0 & i > 1){ #for some length of element 2 if it is 0 and we are considering not the first element
        #then we apply the same algorithm 
        num_children = num_children + c(rep(0, compressed_sum[[i]]), rep(1, v2[1]))
      }
    }
  } 
  #second case when we do not have 5 zeros in a row
  else {
    if (compressed[[2]][1] == 0){
      num_children = num_children + c(rep(0, compressed[[1]][1]), rep(1,v1[1]))
    } else {
      num_children = rep(1, length(vector))
    }
  }
  #this account for the case of a zero vector given to the function
  if (length(vector)==0){
    return(c())}

  return(num_children)
}

how_many_children(c(0,0,0,0,1,1,1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,1)) #checking whether it works

