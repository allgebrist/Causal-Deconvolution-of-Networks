library("acss")
library("igraph")
source("scripts/BDM2D.R")

deconvolve <- function(original_graph, block_size, offset, desired_components = 1) {
  
    while(length(decompose(original_graph)) < desired_components) {
    
        original_matrix   <- as.matrix(as_adjacency_matrix(original_graph))
        original_bdm      <- bdm2D(original_matrix, block_size, offset)
        
        edge_deletions_df <- as_data_frame(original_graph, what = "edges")
        deletion_columns  <- c("bdm_value", "information_loss")
        
        edge_deletions_df[, deletion_columns] <- NA
        
        for(i in 1:nrow(edge_deletions_df)) {
          
            deleted_edge_graph  <- delete_edges(original_graph, 
                                                paste0(edge_deletions_df[i, ]$from,
                                                       "|",edge_deletions_df[i, ]$to))
            
            deleted_edge_matrix <- as.matrix(as_adjacency_matrix(deleted_edge_graph))
            
            deleted_edge_bdm    <- bdm2D(deleted_edge_matrix, block_size, offset)
            
            
            edge_deletions_df[i, ]$bdm_value        <- deleted_edge_bdm
            edge_deletions_df[i, ]$information_loss <- original_bdm - deleted_edge_bdm
  
        }
        
        no_info_gain <- edge_deletions_df$information_loss > 0
        minimal_loss <- min(edge_deletions_df[no_info_gain, ]$information_loss)
        
        minloss_elements <- edge_deletions_df$information_loss == minimal_loss
        
        edge_deletions_df <- edge_deletions_df[minloss_elements, ]
        
        for(i in 1:nrow(edge_deletions_df)) {
          
            original_graph  <- delete_edges(original_graph, 
                                                paste0(edge_deletions_df[i, ]$from,
                                                       "|",edge_deletions_df[i, ]$to))
          
        }
    
    }
    
    return(original_graph)
  
}




###################################
############ TEST CASE ############
###################################

# start.time <- Sys.time()
# deconvolve(make_graph("Frucht"),4,1,2)
# end.time <- Sys.time()
# time.elapsed <-end.time - start.time
# print(time.elapsed)
