shinyServer(function(input, output, session) {
###########################
###########################
###########################

json.output.file.input <- reactive({
	if (is.null(input$json.output.file)){
		cat(file=stderr(), paste0("no json.output.file yet\n"))
		return(NULL)
	} else {
		temp <- readLines(input$json.output.file$datapath, warn=F)
    cat(file=stderr(), paste0("loading json.output.file with names:", names(temp),"\n"))
	}
})


dataOutput2 <- eventReactive(input$reacButton, {
 temp <- readLines(input$json.output.file$datapath, warn=F)
    cat(file=stderr(), paste0("loading json.output.file with names:", names(temp),"\n"))
})	

dataOutput <- reactive({
		if(is.null(json.output.file.input())) {return(NULL)} else {
			temp <- json.output.file.input()[1,1]
			
		cat(file=stderr(), paste0("reacting:",  temp, "\n"))
		print("yo")
		}
})

}
)



