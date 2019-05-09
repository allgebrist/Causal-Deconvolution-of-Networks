library("acss")
library("igraph")
source("scripts/BDM2D.R")

get_info_signature <- function(original_graph, block_size, offset) {
  
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
    # This condition must be revised
    edge_deletions_df <- edge_deletions_df[edge_deletions_df$information_loss > 0, ]
    edge_deletions_df <- edge_deletions_df[order(-edge_deletions_df$information_loss), ]

    return(edge_deletions_df)
    
}




###################################
############ TEST CASE ############
###################################


###### INFORMATION SIGNATURE ######

# is <- get_info_signature(make_graph("Frucht"),4,4)
# plot(log(is$information_loss)+80, xlab = "edges sorted by max info value", ylab = "log info values (+80)", col = "red")
# lines(log(is$information_loss)+80, col = "red")


######## SEE CUTTING PLACES #######

# diffs <- c()
# for(i in 1:nrow(is)) {
#      if(i != nrow(is)) {
#          diffs <- c(diffs, is$information_loss[i]-is$information_loss[i+1])
#      }
# }
# plot(diffs, xlab = "edges sorted by max info value", ylab = "sequential info differences", col = "blue")
# lines(diffs, col = "blue")
# curve(log2(2)*x^0, col = "purple", add = TRUE)
