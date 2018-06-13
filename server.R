library("shiny")
library("igraph")
source("deconvolve.R")
source("infosignature.R")
source("deconvolveterm.R")
source("scripts/loadGraph.R")

shinyServer(function(input, output, session) {
    
    g <- loadGraphPA("./data/testgraph.csv")
    g_decomposed <- deconvolve_with_termination(g, 4, 4, 0.96)
    
    react_graph <- reactiveValues(g = g, g_decomposed = g_decomposed)
    
    
    observeEvent(input$param_adjustment, {
      
        updateNumericInput(session, "param_adjustment")
    })
    
  
    observeEvent(input$file, {
        
        if(is.null(input$file$datapath)) {} 
        else {react_graph$g <- loadGraphPA(input$file$datapath)}
      
    }, ignoreInit = FALSE) 
    
    
    output$graph_plot <- renderPlot({

        plot(react_graph$g, edge.width = 2, edge.color = "Firebrick1", 
             vertex.color = "Lightblue2", vertex.size = 25, 
             vertex.label.family = "Arial Black")
    })
    
    
    output$graph2_plot <- renderPlot({
        
        react_graph$g_decomposed <- deconvolve_with_termination(react_graph$g, 4, 4, input$param_adjustment)
        
        plot(react_graph$g_decomposed, edge.width = 2, edge.color = "Firebrick1",
             vertex.color = "Lightblue2", vertex.size = 25, 
             vertex.label.family = "Arial Black")
    })
    
    
    output$removed_edges <- renderText({
        
        decomposed_df <- as_data_frame(react_graph$g %m% 
                                         react_graph$g_decomposed, what = "edges")
        
        isolate({
            paste0(decomposed_df$from,"|",decomposed_df$to,"  ")
        })
      
    })
    
    output$info_signature <- renderPlot({
      
        is <- get_info_signature(react_graph$g, 4, 4)
        plot(log(is$information_loss)+80, xlab = "edges sorted by max info value", 
             ylab = "log info values (+80)", main = "Information signature", col = "red")
        lines(log(is$information_loss)+80, col = "red")
      
    })
    
    output$cutting_points <- renderPlot({
      
        is <- get_info_signature(react_graph$g, 4, 4)
      
        diffs <- c()
        
        for(i in 1:nrow(is)) {
              if(i != nrow(is)) {
                  diffs <- c(diffs, is$information_loss[i]-is$information_loss[i+1])
              }
        }
        
        plot(diffs, xlab = "edges sorted by max info value", ylab = "sequential info differences", 
             main = "Cutting points plot", col = "blue")
        
        lines(diffs, col = "blue")
        
        curve(log2(2)*x^0, col = "purple", add = TRUE)
      
    })
    
})