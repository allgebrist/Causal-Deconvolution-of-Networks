library("shiny")
library("igraph")
source("deconvolve.R")

shinyServer(function(input, output, session) {

    g <- make_graph("Frucht")
    g_decomposed <- deconvolve(g, 4, 1, 2)
    
    react_graph <- reactiveValues(g = g, g_decomposed = g_decomposed)
    
    
    observeEvent(input$n_components, {
      
      updateSliderInput(session,
                        "n_components",
                        max = vcount(react_graph$g))
    })
    
  
    observeEvent(input$file, {
      
        in_file <- input$file
        
        if(is.null(inFile$datapath)){} 
        else {react_graph$g <- loadGraphPA(in_file$datapath)}
      
    }, ignoreInit = FALSE) 
    
    
    output$graph_plot <- renderPlot({
        
        input$eval_button

        plot(react_graph$g, edge.width = 2, edge.color = "Firebrick1", 
             vertex.color = "Lightblue2", vertex.size = 25, 
             vertex.label.family = "Arial Black")
    })
    
    
    output$graph2_plot <- renderPlot({
        
        input$eval_button
        
        react_graph$g_decomposed <- deconvolve(react_graph$g, 4, 1, input$n_components)
        
        plot(react_graph$g_decomposed, edge.width = 2, edge.color = "Firebrick1",
             vertex.color = "Lightblue2", vertex.size = 25, 
             vertex.label.family = "Arial Black")
    })
    
    
    #output$removed_edges <- renderText({
      
    #    input$eval_button
    #    isolate({
    #      paste0(E(g %m% g_decomposed))
    #    })
      
    #})
    
})