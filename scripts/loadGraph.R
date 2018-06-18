library("igraph")

load_graph <- function(data_path) {
    
    loaded_df <- read.csv(data_path,
                          header = FALSE,
                          sep=',',
                          quote="'",
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
    
    loaded_df <- loaded_df[sapply(loaded_df, is.numeric)]
    
    rownames(loaded_df) <- colnames(loaded_df)
    
    loaded_mat <- as.matrix(loaded_df)
    
    g <- graph_from_adjacency_matrix(loaded_mat) %>%
            set_vertex_attr("label", value = rownames(loaded_df))
    
    return(g)
}