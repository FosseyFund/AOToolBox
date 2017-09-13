function(input, output) {
###########################
###########################
###########################

json.output.file.input <- reactive({
	if (is.null(input$json.output.file)){
		cat(file=stderr(), paste0("no json.output.file yet\n"))
		return(NULL)
	} else {
		temp <- readLines(input$json.output.file$datapath, warn=F)
    cat(file=stderr(), paste0("loading json.output.file\n"))
	}
})


dataOutput2 <- observeEvent(input$reacButton, {
			#output$value <- renderPrint("test")

 #temp <- readLines(input$json.output.file$datapath, warn=F)
    cat(file=stderr(), paste0("loading json.output.file with names:\n"))
})	

dataOutput <- reactive({
		#cat(file=stderr(), paste0("reacting:",  temp, "\n"))
		#print("yo")

})



}




