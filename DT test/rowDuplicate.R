observeEvent(input$duplicateSessionRow, {
		if(!is.null(isolate(input$sessionsDT_select))) {
      	 if(!sessionsRV()$session_start_timeStamp[isolate(input$sessionsDT_select)]=="" &
      	    !sessionsRV()$device_ID[isolate(input$sessionsDT_select)]==""
      	 ){

      cat(file=stderr(), paste0("duplicating... "))
      dupRow <- sessionsRV()[isolate(input$sessionsDT_select),]
      dupRow$session_start_timeStamp <- paste(dupRow$session_start_timeStamp, "EDIT !")
      tableValues$sessionsTable <- rbind(tableValues$sessionsTable, dupRow)

      output$sessionsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render sessionsDT RowDuplicate", "\n"))
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
  }) 
  
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
  }
  }
  })
 
###########################################
###########################################
###########################################
 
observeEvent(input$duplicateFocalRow, {
    		
		if(!is.null(input$focalsDT_select)) {
		 if(!focalsRV()$focal_start_timeStamp[input$focalsDT_select]==""){

		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRow <- cbind(dupRowSession, dupRowFocal)
      dupRow$focal_start_timeStamp <- paste(dupRow$focal_start_timeStamp, "EDIT !")
      dupRow$focal_end_timeStamp <- paste(dupRow$focal_end_timeStamp, "EDIT !")
      dupRow$focal_individual_ID <- "ENTER FOCAL INDIV ID"
      colnames <- names(tableValues$focalsTable)
      tableValues$focalsTable <- smartbind(tableValues$focalsTable, dupRow)[,colnames]
	
      output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDTDuplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(isolate(emptyFocalListRow()))));
    d3tf(isolate(focalsRV()),
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
      }
      }
})  
 
###########################################
###########################################
########################################### 

observeEvent(input$duplicateBehaviorRow, {
    		
		if(!is.null(input$behaviorsDT_select)) {
			if(!behaviorsRV()$behavior_time[input$behaviorsDT_select]=="" & 
			!behaviorsRV()$actor[input$behaviorsDT_select]=="" &
			!behaviorsRV()$subject[input$behaviorsDT_select]=="") {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[isolate(input$sessionsDT_select),]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowBehav <- behaviorsRV()[input$behaviorsDT_select,]

      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowBehav)
      dupRow$behavior_time <- paste(dupRow$behavior_time, "EDIT !")
      dupRow$actor <- "ENTER ACTOR"
      dupRow$subject <- "ENTER SUBJECT"
      colnames <- names(tableValues$behaviorsTable)
      tableValues$behaviorsTable <- smartbind(tableValues$behaviorsTable, dupRow)[,colnames]
      
     output$behaviorsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render behaviorsDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyBehaviorRow()))
    ));
    d3tf(isolate(behaviorsRV()),
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
	}
    }
})   

###########################################
###########################################
###########################################

observeEvent(input$duplicateScanListRow, {
		if(!is.null(input$scanListDT_select)){
			if(!scanListRV()$scan_time[input$scanListDT_select]==""){
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[isolate(input$sessionsDT_select),]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowScanList <- scanListRV()[input$scanListDT_select,]

      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList)
      dupRow$scan_time <- paste(dupRow$scan_time, "EDIT !")
      colnames <- names(tableValues$scansTable)
      tableValues$scansTable <- smartbind(tableValues$scansTable, dupRow)[,colnames]
      
      output$scanListDT <- renderD3tf({
						     cat(file=stderr(), paste0("render scanListDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanListRow()))
    ));
    d3tf(isolate(scanListRV()),
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
      }
      }
}) 
    
###########################################
###########################################
###########################################

observeEvent(input$duplicateScanRow, {
    		
		if(!is.null(input$scansDT_select)){
			if(!scansRV()$scanned_individual_ID[input$scansDT_select]=="") {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[isolate(input$sessionsDT_select),]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowScanList <- scanListRV()[input$scanListDT_select,]
      dupRowScan <- scansRV()[input$scansDT_select,]

      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList, dupRowScan)
      dupRow$scanned_individual_ID <- paste(dupRow$scanned_individual_ID, "EDIT !")

      colnames <- names(tableValues$scansTable)
      tableValues$scansTable <- smartbind(tableValues$scansTable, dupRow)[,colnames]
  	  
  
  
  	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(isolate(scansRV()),
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
      }
      }
}) 

###########################################
###########################################
########################################### 

observeEvent(input$duplicateDayVarsRow, {
    		
		if(!is.null(input$dayVarsDT_select)) {
			if(!dayVarsRV()$dayVars[input$dayVarsDT_select]=="") {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[isolate(input$sessionsDT_select),]
      dupRowDayVars <- dayVarsRV()[input$dayVarsDT_select,]

      dupRow <- cbind(dupRowSession, dupRowDayVars)
      dupRow$dayVars <- paste(dupRow$dayVars, "EDIT !")
      colnames <- names(tableValues$dayVarsTable)
      tableValues$dayVarsTable <- smartbind(tableValues$dayVarsTable, dupRow)[,colnames]
      
     output$dayVarsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render dayVarsDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyDayVarsRow()))
    ));
    d3tf(isolate(dayVarsRV()),
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
	}
    }
})

###########################################
###########################################
########################################### 

observeEvent(input$duplicateCommentsRow, {
    		
		if(!is.null(input$commentsDT_select)) {
			if(!commentsRV()$comment_timeStamp[input$commentsDT_select]=="") {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[isolate(input$sessionsDT_select),]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowComment <- commentsRV()[input$commentsDT_select,]
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowComment)

      dupRow$comment_timeStamp <- paste(dupRow$comment_timeStamp, "EDIT !")
      colnames <- names(tableValues$commentsTable)
      tableValues$commentsTable <- smartbind(tableValues$commentsTable, dupRow)[,colnames]
      
     output$commentsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render commentsDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyCommentRow()))
    ));
    d3tf(isolate(commentsRV()),
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
	}
    }
})

###########################################
###########################################
########################################### 

observeEvent(input$duplicateBackgroundTapsRow, {
    		
		if(!is.null(input$backgroundTapsDT_select)) {
			if(! backgroundTapsRV()$backgroundTap_timeStamp[input$backgroundTapsDT_select]=="") {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[isolate(input$sessionsDT_select),]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowBackGroundTap <- backgroundTapsRV()[input$backgroundTapsDT_select,]
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowBackGroundTap)

      dupRow$backgroundTap_timeStamp <- paste(dupRow$backgroundTap_timeStamp, "EDIT !")
      colnames <- names(tableValues$backgroundTapsTable)
      tableValues$backgroundTapsTable <- smartbind(tableValues$backgroundTapsTable, dupRow)[,colnames]
      
     output$backgroundTapsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render backgroundTapsDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyBackgroundTapsRow()))
    ));
    d3tf(isolate(backgroundTapsRV()),
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
	}
    }
})

###########################################
###########################################
########################################### 

observeEvent(input$duplicateFocalVarsRow, {
    		
		if(!is.null(input$focalVarsDT_select)) {
			if(!focalVarsRV()$focalVars[input$focalVarsDT_select]=="") {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[isolate(input$sessionsDT_select),]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowFocalVars <- focalVarsRV()[input$focalVarsDT_select,]
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowFocalVars)

      dupRow$focalVars <- paste(dupRow$focalVars, "EDIT !")
      colnames <- names(tableValues$focalVarsTable)
      tableValues$focalVarsTable <- smartbind(tableValues$focalVarsTable, dupRow)[,colnames]
      
     output$focalVarsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render focalVarsDT Duplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyFocalVarsRow()))
    ));
    d3tf(isolate(focalVarsRV()),
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
	}
    }
})

###########################################
###########################################
########################################### 

observeEvent(input$duplicateContinuousVarsRow, {
    		
		if(!is.null(input$continuousVarsDT_select)) {
			if(! continuousVarsRV()$continuousVars[input$continuousVarsDT_select]=="") {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[isolate(input$sessionsDT_select),]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowContinuousVars <- continuousVarsRV()[input$continuousVarsDT_select,]
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowContinuousVars)

      dupRow$dupRowContinuousVars <- paste(dupRow$dupRowContinuousVars, "EDIT !")
      colnames <- names(tableValues$continuousVarsTable)
      tableValues$continuousVarsTable <- smartbind(tableValues$continuousVarsTable, dupRow)[,colnames]
      
     output$continuousVarsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render continuousVarsDT Duplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyContinuousVarsRow()))
    ));
    d3tf(isolate(continuousVarsRV()),
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
	}
    }
})


###########################################
###########################################
########################################### 

observeEvent(input$duplicateScanVarsRow, {
    		
		if(!is.null(input$scanVarsDT_select)) {
			if(!scanVarsRV()$scanVars[input$scanVarsDT_select]=="") {
		
	  cat(file=stderr(), paste0("duplicating... "))


      dupRowSession <- sessionsRV()[isolate(input$sessionsDT_select),]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowScanList <- scanListRV()[input$scanListDT_select,]
      dupRowScanVar <- scanVarsRV()[input$scanVarsDT_select,]
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList, dupRowScanVar)
      dupRow$scanVars <- paste(dupRow$scanVars, "EDIT !")

      colnames <- names(tableValues$scanVarsTable)
      tableValues$scanVarsTable <- smartbind(tableValues$scanVarsTable, dupRow)[,colnames]
      
     output$scanVarsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render scanVarsDT Duplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanVarsRow()))
    ));
    d3tf(isolate(scanVarsRV()),
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
	}
    }
})