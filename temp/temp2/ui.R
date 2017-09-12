fluidPage(
    
  # Copy the line below to make a checkbox
  checkboxInput("checkbox", label = "Choice A", value = TRUE),
  	    actionButton("reacButton", label = "Press here!"),

  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
  
)
