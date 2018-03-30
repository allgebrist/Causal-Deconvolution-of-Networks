library("shiny")
library("shinythemes")

orange_slider <- "
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
background: #f63;
border-color: #f63;
}"

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
            
            sliderInput(inputId = "n_components",
                        label = "Number of desired subcomponents",
                        min = 1, max = 10, value = 1, step = 1),
            
            actionButton("eval_button", "Evaluate", 
                         style = "color: #fff; 
                         background-color: #f63; 
                         border-color: #f63")),
        
        mainPanel(
            
            fluidRow(
              column(width = 6,
                     HTML('<center><h3>Original graph</h3></center>'),
                     plotOutput("graph_plot")),
              column(width = 6,
                     HTML('<center><h3>Decomposed graph</h3></center>'),
                     plotOutput("graph2_plot"))
            ))
      
      )
  
))