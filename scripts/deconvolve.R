library("acss")
library("igraph")

source("scripts/BDM2D.R")

deconvolve <- function(original_graph, blockSize, offset) {
  
    original_matrix   <- as.matrix(as_adjacency_matrix(original_graph))
    original_bdm      <- bdm2D(original_matrix, blockSize = blockSize, offset = offset)
    
    edge_deletions_df <- as_data_frame(original_graph, what = "edges")
    deletion_columns  <- c("bdm_value", "information_loss")
    
    edge_deletions_df[, deletion_columns] <- NA
    
    for(i in 1:nrow(edge_deletions_df)) {
        
        deleted_edge_graph  <- delete_edges(original_graph, 
                                           paste0(edge_deletions_df[i, ]$from,
                                                  "|",edge_deletions_df[i, ]$to))
        
        deleted_edge_matrix <- as.matrix(as_adjacency_matrix(deleted_edge_graph))
        
        deleted_edge_bdm    <- bdm2D(deleted_edge_matrix, blockSize = blockSize, offset = offset)
        
        
        edge_deletions_df[i, ]$bdm_value        <- deleted_edge_bdm
        edge_deletions_df[i, ]$information_loss <- original_bdm - deleted_edge_bdm
    }
    
    return(edge_deletions_df)
}