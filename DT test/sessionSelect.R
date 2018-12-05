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
# cat(file=stderr(), paste0("list of output objects: ", names(outputOptions(output)), "\n"))


  
# cat(file=stderr(), paste0("list of input objects: ",sort(names(reactiveValuesToList(output))), "\n"))

 output$sessionsDT <- isolate(renderD3tf({
	 cat(file=stderr(), paste0("render sessionsDTServer", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(isolate(sessionsRV()))
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
  
output$dayVarsDT <- isolate(renderD3tf({
				     cat(file=stderr(), paste0("render dayVarsDT", "\n"))
				     #cat(file=stderr(), paste0("ncol(emptyDayVarsRow()) = ", ncol(isolate(emptyDayVarsRow())),"\n"))
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
  })	)
 
  output$focalsDT <- isolate(renderD3tf({
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
  }))
  
	output$behaviorsDT <- isolate(renderD3tf({
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
  }))

output$commentsDT <- isolate(renderD3tf({
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
  }))

output$backgroundTapsDT <- isolate(renderD3tf({
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
  }))

output$focalVarsDT <- isolate(renderD3tf({
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
  }))

output$continuousVarsDT <- isolate(renderD3tf({
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
  }))
	
	output$scanListDT <- isolate(renderD3tf({
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
  }))

output$scanVarsDT <- isolate(renderD3tf({
		cat(file=stderr(), paste0("render scanVarsDTOK1", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanVarsRow()))));
      		cat(file=stderr(), paste0("render scanVarsDTOK2", "\n"))

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
  }))

	output$scansDT <- isolate(renderD3tf({
								     cat(file=stderr(), paste0("render scansDT", "\n"))

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
  }) ) 


})