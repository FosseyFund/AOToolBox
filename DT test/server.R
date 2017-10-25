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

tableValuesCopy <- reactiveValues(
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

sessionChoices <- reactiveValues(choiceList=NULL)
sessionSelected <- reactiveValues(index=1)


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

###########################
observeEvent(input$sessionSelect, {
	#if(is.null(input$sessionSelect) | is.null(sessionSelected$index)) return(NULL)
	#if(as.numeric(input$sessionSelect)== sessionSelected$index) return(NULL)
	#cat(file=stderr(), paste0("number of session rows0:", nrow(tableValues$sessionsTable),";\n"))
	sessionSelected$index <- as.numeric(input$sessionSelect)

	tableValuesCopy$sessionsTable <-  rbind(tableValuesCopy$sessionsTable, tableValues$sessionsTable)
	if(!is.null(tableValuesCopy$sessionsTable)) tableValuesCopy$sessionsTable <- tableValuesCopy$sessionsTable[order(tableValuesCopy$sessionsTable[,1], tableValuesCopy$sessionsTable[,2]),]
	tableValuesCopy$focalsTable <- rbind(tableValuesCopy$focalsTable, tableValues$focalsTable)
	tableValuesCopy$behaviorsTable <- rbind(tableValuesCopy$behaviorsTable, tableValues$behaviorsTable)
	tableValuesCopy$scansTable <- rbind(tableValuesCopy$scansTable, tableValues$scansTable)
	tableValuesCopy$backgroundTapsTable <- rbind(tableValuesCopy$backgroundTapsTable, tableValues$backgroundTapsTable)
	tableValuesCopy$commentsTable <- rbind(tableValuesCopy$commentsTable, tableValues$commentsTable)
	tableValuesCopy$dayVarsTable <- rbind(tableValuesCopy$dayVarsTable, tableValues$dayVarsTable)
	tableValuesCopy$focalVarsTable <- rbind(tableValuesCopy$focalVarsTable, tableValues$focalVarsTable)
	tableValuesCopy$continuousVarsTable <- rbind(tableValuesCopy$continuousVarsTable, tableValues$continuousVarsTable)
	tableValuesCopy$scanVarsTable <- rbind(tableValuesCopy$scanVarsTable, tableValues$scanVarsTable)


	if (!input$sessionSelect==1) {
	device <- sessionChoices$choiceList[as.numeric(input$sessionSelect)-1,1]
	sessionstarttime <- sessionChoices$choiceList[as.numeric(input$sessionSelect)-1,2]
	
	tableValues$sessionsTable <- tableValuesCopy$sessionsTable[tableValuesCopy$sessionsTable$device_ID==device & tableValuesCopy$sessionsTable$session_start_timeStamp== sessionstarttime,]
	tableValues$focalsTable <- tableValuesCopy$focalsTable[tableValuesCopy$focalsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$focalsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp,]
	tableValues$behaviorsTable <- tableValuesCopy$behaviorsTable[tableValuesCopy$behaviorsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$behaviorsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp,]
	tableValues$scansTable <- tableValuesCopy$scansTable[tableValuesCopy$scansTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$scansTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp,]
	tableValues$backgroundTapsTable <- tableValuesCopy$backgroundTapsTable[tableValuesCopy$backgroundTapsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$backgroundTapsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp,]
	tableValues$commentsTable <- tableValuesCopy$commentsTable[tableValuesCopy$commentsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$commentsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp,]
	tableValues$dayVarsTable <- tableValuesCopy$dayVarsTable[tableValuesCopy$dayVarsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$dayVarsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp,]
	tableValues$focalVarsTable <- tableValuesCopy$focalVarsTable[tableValuesCopy$focalVarsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$focalVarsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp,]
	tableValues$continuousVarsTable <- tableValuesCopy$continuousVarsTable[tableValuesCopy$continuousVarsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$continuousVarsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp,]
	tableValues$scanVarsTable <- tableValuesCopy$scanVarsTable[tableValuesCopy$scanVarsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$scanVarsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp,]
	
	tableValuesCopy$sessionsTable <- tableValuesCopy$sessionsTable[!(tableValuesCopy$sessionsTable$device_ID==device & tableValuesCopy$sessionsTable$session_start_timeStamp==sessionstarttime),]
	tableValuesCopy$focalsTable <- tableValuesCopy$focalsTable[!(tableValuesCopy$focalsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$focalsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp),]
	tableValuesCopy$behaviorsTable <- tableValuesCopy$behaviorsTable[!(tableValuesCopy$behaviorsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$behaviorsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp),]
	tableValuesCopy$scansTable <- tableValuesCopy$scansTable[!(tableValuesCopy$scansTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$scansTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp),]
	tableValuesCopy$backgroundTapsTable <- tableValuesCopy$backgroundTapsTable[!(tableValuesCopy$backgroundTapsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$backgroundTapsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp),]
	tableValuesCopy$commentsTable <- tableValuesCopy$commentsTable[!(tableValuesCopy$commentsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$commentsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp),]
	tableValuesCopy$dayVarsTable <- tableValuesCopy$dayVarsTable[!(tableValuesCopy$dayVarsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$dayVarsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp),]
	tableValuesCopy$focalVarsTable <- tableValuesCopy$focalVarsTable[!(tableValuesCopy$focalVarsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$focalVarsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp),]
	tableValuesCopy$continuousVarsTable <- tableValuesCopy$continuousVarsTable[!(tableValuesCopy$continuousVarsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$continuousVarsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp),]
	tableValuesCopy$scanVarsTable <- tableValuesCopy$scanVarsTable[!(tableValuesCopy$scanVarsTable$device_ID%in%tableValues$sessionsTable$device_ID & tableValuesCopy$scanVarsTable$session_start_timeStamp%in%tableValues$sessionsTable$session_start_timeStamp),]
	
} else {

	tableValuesCopy$sessionsTable -> tableValues$sessionsTable
	tableValuesCopy$focalsTable -> tableValues$focalsTable
	tableValuesCopy$behaviorsTable -> tableValues$behaviorsTable
	tableValuesCopy$scansTable -> tableValues$scansTable
	tableValuesCopy$backgroundTapsTable -> tableValues$backgroundTapsTable
	tableValuesCopy$commentsTable -> tableValues$commentsTable
	tableValuesCopy$dayVarsTable -> tableValues$dayVarsTable
	tableValuesCopy$focalVarsTable -> tableValues$focalVarsTable
	tableValuesCopy$continuousVarsTable -> tableValues$continuousVarsTable
	tableValuesCopy$scanVarsTable -> tableValues$scanVarsTable
	
	tableValuesCopy$sessionsTable <- tableValuesCopy$sessionsTable[0,]
	tableValuesCopy$focalsTable <- tableValuesCopy$focalsTable[0,]
	tableValuesCopy$behaviorsTable <- tableValuesCopy$behaviorsTable[0,]
	tableValuesCopy$scansTable <- tableValuesCopy$scansTable[0,]
	tableValuesCopy$backgroundTapsTable <- tableValuesCopy$backgroundTapsTable[0,]
	tableValuesCopy$commentsTable <- tableValuesCopy$commentsTable[0,]
	tableValuesCopy$dayVarsTable <- tableValuesCopy$dayVarsTable[0,]
	tableValuesCopy$focalVarsTable <- tableValuesCopy$focalVarsTable[0,]
	tableValuesCopy$continuousVarsTable <- tableValuesCopy$continuousVarsTable[0,]
	tableValuesCopy$scanVarsTable <- tableValuesCopy$scanVarsTable[0,]

	
}

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
  
output$dayVarsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render dayVarsDT", "\n"))
				     cat(file=stderr(), paste0("ncol(emptyDayVarsRow()) = ", ncol(isolate(emptyDayVarsRow())),"\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(isolate(emptyDayVarsRow()))));
    d3tf(isolate(emptyDayVarsRow()),
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
 
  output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDTDuplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(isolate(emptyFocalListRow()))));
    d3tf(isolate(emptyFocalListRow()),
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
  
	output$behaviorsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render behaviorsDTDuplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyBehaviorRow()))
    ));
    d3tf(isolate(emptyBehaviorRow()),
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

output$commentsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render commentsDT", "\n"))
	if(is.null(isolate(emptyCommentRow()))) return(NULL)
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyCommentRow())))
    ));
    d3tf(isolate(emptyCommentRow()),
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

output$backgroundTapsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render backgroundTapsDT", "\n"))
	if(is.null(isolate(emptyBackgroundTapsRow()))) return(NULL)

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyBackgroundTapsRow())))
    ));
    d3tf(isolate(emptyBackgroundTapsRow()),
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

output$focalVarsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render focalVarsDT", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyFocalVarsRow())))
    ));
    d3tf(isolate(emptyFocalVarsRow()),
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

output$continuousVarsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render continuousVarsDT", "\n"))
if(is.null(isolate(emptyContinuousVarsRow()))) return(NULL)
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyContinuousVarsRow())))
    ));
    d3tf(isolate(emptyContinuousVarsRow()),
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
	
	output$scanListDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scanListsDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanListRow()))
    ));
    d3tf(isolate(emptyScanListRow()),
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

output$scanVarsDT <- renderD3tf({
		cat(file=stderr(), paste0("render scanVarsDT", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanVarsRow()))
    ));
    d3tf(isolate(emptyScanVarsRow()),
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

	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(isolate(emptyScanRow()),
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


})

###########################
###########################
###########################
source("create_empty_tables.R", local=TRUE)
source("tablesRV.R", local=TRUE)

###########################
###########################
###########################
observeEvent(input$VisualizeData, {

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

	tableValuesCopy$sessionsTable <- tableValues$sessionsTable[0,]
	tableValuesCopy$focalsTable <- tableValues$focalsTable[0,]
	tableValuesCopy$behaviorsTable <- tableValues$behaviorsTable[0,]
	tableValuesCopy$scansTable <- tableValues$scansTable[0,]
	tableValuesCopy$backgroundTapsTable <- tableValues$backgroundTapsTable[0,]
	tableValuesCopy$commentsTable <- tableValues$commentsTable[0,]
	tableValuesCopy$dayVarsTable <- tableValues$dayVarsTable[0,]
	tableValuesCopy$focalVarsTable <- tableValues$focalVarsTable[0,]
	tableValuesCopy$continuousVarsTable <- tableValues$continuousVarsTable[0,]
	tableValuesCopy$scanVarsTable <- tableValues$scanVarsTable[0,]



	if(!is.null(tableValues$sessionsTable)) {
	if(nrow(tableValues$sessionsTable)>0) {
	sessionList <- as.list(1:(nrow(tableValues$sessionsTable)+1))
	names(sessionList) <- c("ALL", paste(tableValues$sessionsTable[,1], tableValues$sessionsTable[,2], sep=' | '))
	sessionChoices$choiceList <- data.frame(tableValues$sessionsTable[,1], tableValues$sessionsTable[,2])
	updateSelectInput(session=session, inputId='sessionSelect', label = "Select a session", choices = sessionList, selected = 1)
	sessionSelected$index <- 1
	}
	}
	
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
  }
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
     
##############postgres connection
DBname <- reactive({
	return(input$postgresDBname)
})

DBuser <- reactive({
	return(input$postgresUser)
})

DBhost <- reactive({
	return(input$postgresHost)
})

DBpwd <- reactive({
	return(input$postgresPwd)
})

DBport <- reactive({
	return(input$postgresPort)
})

database <- eventReactive(input$postgresConnect, {
	#cat(file=stderr(), paste(DBname(), DBhost(), DBport(), DBuser(), DBpwd(), collapse=", "))
	if(is.null(DBname()) | is.null(DBuser()) | is.null(DBhost()) | is.null(DBpwd()) | is.null(DBport())) return(NULL)
    
    drv <- dbDriver("PostgreSQL")
    	all_cons <- dbListConnections(drv)
    for(con in all_cons) dbDisconnect(con)

    con <- dbConnect(dbDriver("PostgreSQL"), dbname = tolower(DBname()), host = DBhost(), port = DBport(), user = DBuser(), password = DBpwd())
#cat(file=stderr(), paste(DBname(), DBhost(), DBport(),DBuser(), DBpwd(), collapse=", "))
    return(con)
})

connectionStatus <- reactiveValues(state=FALSE)

output$DoneConnect <- renderText({
	if(is.null(database())){
		connectionStatus$state <- FALSE
		return(NULL)
		}
		connectionStatus$state <- TRUE
		return("SUCCESS!")	
})

#############unzip and upload


observeEvent(input$runZipUpload, {
   if (is.null(input$zipFolder)) return(NULL)
   fileNames <- unzip(input$zipFolder$datapath)
   ans <- list()
   for(i in 1:length(fileNames)){
   	ans[[i]] <- read.csv(fileNames[i], header=T, check.names=F, stringsAsFactors=F)
   }
   names(ans) <- unlist(strsplit(unlist(strsplit(fileNames, split="./")), ".csv"))
   cat(file=stderr(), paste0("files extracted: ", paste(names(ans), collapse=";"),"\n"))
   if(connectionStatus$state==TRUE){
   	    cat(file=stderr(), "Uploading file...\n")
con <- database()
if(nrow(ans$sessionsTable)>0) uploadSessionsTable(ans$sessionsTable, con)
if(nrow(ans$focalsTable)>0) uploadFocalsTable(ans$focalsTable, con)
if(nrow(ans$behaviorsTable)>0) uploadBehaviorsTable(ans$behaviorsTable, con)
if(nrow(ans$scansTable)>0) uploadScansTable(ans$scansTable, con)
if(nrow(ans$scansTable)>0) uploadScanData(ans$scansTable, con)
if(nrow(ans$scansTable)>0) uploadScansIntermediateTables(ans$scansTable, con)
if(nrow(ans$behaviorsTable)>0) uploadBehaviorsIntermediateTables(ans$behaviorsTable, con)
if(nrow(ans$scanVarsTable)>0) uploadScanVariables(ans$scanVarsTable, con)
if(nrow(ans$scanVarsTable)>0) uploadscanVarsIntermediateTables(ans$scanVarsTable, con)

if("continuous_focal_variables" %in% dbListTables(con) & nrow(ans$continuousVarsTable)>0) {
uploadContinuousVariables(ans$continuousVarsTable, con)
uploadContinuousVarsIntermediateTables(ans$continuousVarsTable, con)
}
if(nrow(ans$scanVarsTable)>0) uploadFocalVariables(ans$focalVarsTable, con)
if(nrow(ans$scanVarsTable)>0) uploadfocalVarsIntermediateTables(ans$focalVarsTable, con)
if(nrow(ans$dayVarsTable)>0) uploadSessionVariables(ans$dayVarsTable, con)
if(nrow(ans$dayVarsTable)>0) uploadSessionVarsIntermediateTables(ans$dayVarsTable, con)
if(nrow(ans$backgroundTapsTable)>0) uploadBackgroundTapsTable(ans$backgroundTapsTable, con)
if(nrow(ans$commentsTable)>0) uploadCommentTable(ans$commentsTable, con)

output$DoneUploading <- renderText({
	cat(file=stderr(), "Success!\n")
		return("SUCCESS!")	
})
   }
})


##############
}
)



