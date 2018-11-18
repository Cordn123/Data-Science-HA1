rm(list=ls())

how_many_children <- function(vector){
  #параметры, которые нужны на протяжении всей функции
  num_children = rep(0, length(vector))
  compressed = rle(vector) 
  v1 <- diff(c(compressed[[1]][1], length(vector)))
  
  #первый кейс, когда где-то есть 5 нулей подряд
  if (length(which(compressed[[1]] >= 5)) != 0) {
    detect_new_child = which(compressed[[1]]>=5)
    compressed_sum = cumsum(compressed[[1]])
    num_children = rep(0, length(vector))
    
    if (compressed[[2]][1]==0){
      num_children = num_children + c(rep(0,compressed[[1]][1]), rep(1,v1[1]))
    } else {
      num_children = num_children + rep(1, length(vector))  
    } 
    
    for (i in detect_new_child){
      v2 <- diff(c(compressed_sum[[i]], length(vector)))
      if(compressed[[2]][i] == 0 & i > 1){
        num_children = num_children + c(rep(0, compressed_sum[[i]]), rep(1, v2[1]))
      }
    }
    
  } 
  
  #второй кейс, когда нет 5 нулей подряд ни в одном месте
  else {
    if (compressed[[2]][1] == 0){
      num_children = num_children + c(rep(0, compressed[[1]][1]), rep(1,v1[1]))
      
    } else {
      num_children = rep(1, length(vector))
      
    }
    
  }
  if (length(vector)==0){
    return(c())}
  
  return(num_children)
}
how_many_children(c(0,0,0,0,1,1,1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,1)) 
