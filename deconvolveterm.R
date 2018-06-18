library("acss")
library("igraph")
source("infosignature.R")

deconvolve_with_termination <- function(original_graph, block_size, offset, epsilon = 1) {
    
    information_signature <- get_info_signature(original_graph, block_size, offset)
    information_differences <- c()
    
    for(i in 1:nrow(information_signature)) {
      
        if(i != nrow(information_signature)) {
          
          information_differences <- c(information_differences, 
                                       information_signature$information_loss[i]-information_signature$information_loss[i+1])
        
        }
      
    }
    
    cutting_points <- c()
    
    for(i in 1:length(information_differences)) {
      
        if(abs(information_differences[i]-log2(2)) > epsilon) {
          cutting_points <- c(cutting_points, i+1)
          
        }
      
    }
      
    for(i in 1:length(cutting_points)) {
      
        if(length(cutting_points) > 0) {
          
            original_graph  <- delete_edges(original_graph, 
                                            paste0(information_signature[cutting_points[i], ]$from,
                                                   "|", information_signature[cutting_points[i], ]$to))
        }

    }
    
    return(original_graph)
  
}