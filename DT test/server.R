shinyServer(function(input, output, session) {


###########################
###########################
###########################
tableValues <- reactiveValues(dataOutput=NULL)

json.output.file.input <- reactive({
	if (is.null(input$json.output.file)) return(NULL)
	else return(readLines(input$json.output.file$datapath, warn=F))
})	

behaviors.json.input <- reactive({
    if (is.null(input$behaviors.json)) return(NULL)
    else return(readLines(input$behaviors.json$datapath, warn=F))
})

layout_info.json.input <- reactive({
    if (is.null(input$layout_info.json)) return(NULL)
    else return(readLines(input$layout_info.json$datapath, warn=F))
})

observeEvent({
	json.output.file.input()
	behaviors.json.input()
	layout_info.json.input()
	}, {
	if(is.null(json.output.file.input()) | is.null(behaviors.json.input()) | is.null(layout_info.json.input())) {
	tableValues$dataOutput <- NULL
	} else {
	tableValues$dataOutput <- jsonOutputConversion(json.output.file.input(), behaviors.json.input(), layout_info.json.input(), colmerge=input$colmerge)
}
}
)

# observeEvent(dataOutput(), {
	# output$value <- renderPrint({paste0("dataOutput()$sessionsTable : ", paste(names(dataOutput()$sessionsTable), collapse="; "))
		# })
# })	    



###########################
###########################
###########################
source("create_empty_tables.R", local=TRUE)
source("tablesRV.R", local=TRUE)

###########################
###########################
###########################

output$sessionsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render sessionsDT", "\n"))

	#temp <- is.null(input$deleteSessionRow)###makes function reactive to deletion
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptySessionRow())
    ));
    d3tf(isolate(sessionsRV()),
         tableProps = isolate(tableProps),
         extensions = list(
           list(name = "sort")
         ),
         showRowNames = FALSE,
         tableStyle = "table table-bordered",
         edit = TRUE,
         selectableRows='single',
         selectableRowsClass='success'
	);
  })

###########################    		
###########################
#################select rows
###########################
###########################
source("tableSelect.R", local=TRUE)
    		
###########################    		
###########################
#################edit cells
###########################
###########################
source("cellEdit.R", local=TRUE)
     
#########################   
#########################   
######row deletion
#########################   
#########################
source("rowDelete.R", local=TRUE)

#########################   
#########################   
######row duplication
#########################   
#########################
source("rowDuplicate.R", local=TRUE)


#########################   
    
    # output$downloadBehaviorsView <- downloadHandler(
    # filename = function() { 
		 # paste('BehaviorsView.csv', sep='') 
	 # },
    # content = function(file) {
     # write.csv(views$dat1, file, row.names=F, na="")
    # }
  	# )
    # output$downloadScansView <- downloadHandler(
    # filename = function() { 
		 # paste('ScansView.csv', sep='') 
	 # },
    # content = function(file) {
     # write.csv(views$dat2, file, row.names=F, na="")
    # }
  # )
  }
)



