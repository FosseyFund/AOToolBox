function(input, output) {

  # You can access the value of the widget with input$checkbox, e.g.
  output$value <- renderPrint({ input$reacButton })

dataOutput2 <- observeEvent(input$reacButton, {
			output$value <- renderPrint({2==3})

 #temp <- readLines(input$json.output.file$datapath, warn=F)
    cat(file=stderr(), paste0("loading json.output.file with names:\n"))
})	
}
