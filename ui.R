library("shiny")
library("shinythemes")

orange_slider <- ".irs-bar,
                  .irs-bar-edge,
                  .irs-single,
                  .irs-grid-pol { background: #f63;
                                  border-color: #f63; }"

shinyUI(fluidPage(
  theme = shinytheme("united"),
  tags$style(orange_slider),
  
    sidebarLayout(
      
        wellPanel(
            fileInput(inputId = "file",
                      label = "Choose a CSV file",
                      accept = c('text/comma-separated-values',
                                 'text/plain',
                                 'text/csv',
                                 '.csv')),
            
            numericInput(inputId = "param_adjustment",
                        label = "Adjust cutting parameter*",
                        value = 0.96, step = 0.1)),
        
        mainPanel(
            
            fluidRow(
                HTML('<center><h2>Causal separation results</h2></center>'),
                column(width = 6,
                       HTML('<center><h3>Original graph</h3></center>'),
                       plotOutput("graph_plot")),
                column(width = 6,
                       HTML('<center><h3>Deconvolved graph</h3></center>'),
                       plotOutput("graph2_plot")),
                div(p("Removed edges: ", textOutput(outputId = "removed_edges")),
                    style = "font-size:120%",
                    align = "center"),
                br(),
                hr(),
                br(),
                HTML('<center><h2>Information properties</h2></center>'),
                column(width = 6,
                       plotOutput("info_signature")),
                column(width = 6,
                       plotOutput("cutting_points")),
                div(p("*This is the absolute distance between the differences 
                      of consecutive values in the information signature 
                      (the list of information values of all edges 
                      sorted by maximum contribution) and log(2)."),
                    style = "font-size:90%",
                    align = "justify")
            ))
        
      )
  
))