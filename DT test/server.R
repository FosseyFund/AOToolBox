shinyServer(function(input, output, session) {


###########################
###########################
###########################
tableValues <- reactiveValues(
						sessionsTable=NULL, 
						focalsTable=NULL, 
						behaviorsTable=NULL, 
						scansTable=NULL,
						backgroundTapsTable=NULL,
						commentsTable=NULL,
						dayVarsTable=NULL,
						focalVarsTable=NULL,
						continuousVarsTable=NULL,
						scanVarsTable=NULL
				)


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
	tableValues$sessionsTable <- NULL
	tableValues$focalsTable <- NULL
	tableValues$behaviorsTable <- NULL
	tableValues$scansTable <- NULL
	} else {
	dataOutput <- jsonOutputConversion(json.output.file.input(), behaviors.json.input(), layout_info.json.input(), colmerge=input$colmerge)
	tableValues$sessionsTable <- dataOutput$sessionsTable
	tableValues$focalsTable <- dataOutput$focalsTable
	tableValues$behaviorsTable <- dataOutput$behaviorsTable
	tableValues$scansTable <- dataOutput$scansTable
	tableValues$backgroundTapsTable <- dataOutput$backgroundTapsTable
	tableValues$commentsTable <- dataOutput$commentsTable
	tableValues$dayVarsTable <- dataOutput$dayVarsTable
	tableValues$focalVarsTable <- dataOutput$focalVarsTable
	tableValues$continuousVarsTable <- dataOutput$continuousVarsTable
	tableValues$scanVarsTable <- dataOutput$scanVarsTable
}
}
)



###########################
###########################
###########################
source("create_empty_tables.R", local=TRUE)
source("tablesRV.R", local=TRUE)

###########################
###########################
###########################
observeEvent(tableValues$sessionsTable, {
output$sessionsDT <- isolate(renderD3tf({
						     cat(file=stderr(), paste0("render sessionsDTServer", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(isolate(emptySessionRow()))
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
  }))
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
    
	output$downloadSessionsTable <- downloadHandler(
    filename = function() { 
		 paste('sessionsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(tableValues$sessionsTable, file, row.names=F, na="")
    }
  )
	output$downloadFocalsTable <- downloadHandler(
    filename = function() { 
		 paste('focalsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(tableValues$focalsTable, file, row.names=F, na="")
    }
  )
	output$downloadBehaviorsTable <- downloadHandler(
    filename = function() { 
		 paste('behaviorsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(tableValues$behaviorsTable, file, row.names=F, na="")
    }
  )
	output$downloadScansTable <- downloadHandler(
    filename = function() { 
		 paste('scansTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(tableValues$scansTable, file, row.names=F, na="")
    }
  )
  	output$downloadBackgroundTapsTable <- downloadHandler(
    filename = function() { 
		 paste('backgroundTapsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(tableValues$backgroundTapsTable, file, row.names=F, na="")
    }
  )
   	output$downloadCommentsTable <- downloadHandler(
    filename = function() { 
		 paste('commentsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(tableValues$commentsTable, file, row.names=F, na="")
    }
  )
  output$downloadDayVarsTable <- downloadHandler(
    filename = function() { 
		 paste('dayVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(tableValues$dayVarsTable, file, row.names=F, na="")
    }
  )
  output$downloadFocalVarsTable <- downloadHandler(
    filename = function() { 
		 paste('focalVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(tableValues$focalVarsTable, file, row.names=F, na="")
    }
  )
  output$downloadContinuousVarsTable <- downloadHandler(
    filename = function() { 
		 paste('continuousVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(tableValues$continuousVarsTable, file, row.names=F, na="")
    }
  )
  output$downloadScanVarsTable <- downloadHandler(
    filename = function() { 
		 paste('scanVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(tableValues$scanVarsTable, file, row.names=F, na="")
    }
  )
  
  output$downloadZip <- downloadHandler(
       filename = function() {
         paste("AO_OutPut_", Sys.time(), ".zip", sep="")
       },
       content = function(fname) {
         fs <- c()
         tmpdir <- tempdir()
         initwd <- getwd()
         setwd(tempdir())


         	write.csv(tableValues$sessionsTable, file=paste0("sessionsTable", ".csv"), row.names=F, na="")
         	write.csv(tableValues$focalsTable, file=paste0("focalsTable", ".csv"), row.names=F, na="")
         	write.csv(tableValues$behaviorsTable, file=paste0("behaviorsTable", ".csv"), row.names=F, na="")
         	write.csv(tableValues$scansTable, file=paste0("scansTable", ".csv"), row.names=F, na="")
         	write.csv(tableValues$backgroundTapsTable, file=paste0("backgroundTapsTable", ".csv"), row.names=F, na="")
         	write.csv(tableValues$commentsTable, file=paste0("commentsTable", ".csv"), row.names=F, na="")
         	write.csv(tableValues$dayVarsTable, file=paste0("dayVarsTable", ".csv"), row.names=F, na="")
         	write.csv(tableValues$focalVarsTable, file=paste0("focalVarsTable", ".csv"), row.names=F, na="")
         	write.csv(tableValues$continuousVarsTable, file=paste0("continuousVarsTable", ".csv"), row.names=F, na="")
         	write.csv(tableValues$scanVarsTable, file=paste0("scanVarsTable", ".csv"), row.names=F, na="")
         	
  
                  
         zip(zipfile=fname, files=paste0(names(tableValues), ".csv"))
       },
       contentType = "application/zip"
     )
}
)



