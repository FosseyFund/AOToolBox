observeEvent(input$duplicateSessionRow, {
		if(!is.null(input$sessionsDT_select)) {
      	 if(!sessionsRV()$session_start_timeStamp[input$sessionsDT_select]=="" &
      	    !sessionsRV()$device_ID[input$sessionsDT_select]==""
      	 ){

      cat(file=stderr(), paste0("duplicating... "))
      dupRow <- sessionsRV()[input$sessionsDT_select,]
      dupRow$session_start_timeStamp <- paste(dupRow$session_start_timeStamp, "EDIT !")
      tableValues$dataOutput$sessionsTable <- smartbind(tableValues$dataOutput$sessionsTable, dupRow)

      output$sessionsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render sessionsDT RowDuplicate", "\n"))
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
  output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDTDuplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyFocalListRow())));
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
    d3tf(emptyBehaviorRow(),
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
    d3tf(emptyScanListRow(),
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
    d3tf(emptyScanRow(),
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
 
 
 
 	observeEvent(input$duplicateFocalRow, {
    		
		if(!is.null(input$focalsDT_select)) {
		 if(!focalsRV()$focal_start_timeStamp[input$focalsDT_select]==""){

		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[input$sessionsDT_select,]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRow <- cbind(dupRowSession, dupRowFocal)
      dupRow$focal_start_timeStamp <- paste(dupRow$focal_start_timeStamp, "EDIT !")
      dupRow$focal_end_timeStamp <- paste(dupRow$focal_end_timeStamp, "EDIT !")
      dupRow$focal_individual_ID <- "ENTER FOCAL INDIV ID"
      tableValues$dataOutput$focalsTable <- smartbind(tableValues$dataOutput$focalsTable, dupRow)

		
      output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDTDuplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyFocalListRow())));
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
    d3tf(emptyBehaviorRow(),
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
    d3tf(emptyScanListRow(),
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
    d3tf(emptyScanRow(),
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
 
 
 	observeEvent(input$duplicateBehaviorRow, {
    		
		if(!is.null(input$behaviorsDT_select)) {
			if(!behaviorsRV()$behavior_time[input$behaviorsDT_select]=="" & 
			!behaviorsRV()$actor[input$behaviorsDT_select]=="" &
			!behaviorsRV()$subject[input$behaviorsDT_select]=="") {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[input$sessionsDT_select,]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowBehav <- behaviorsRV()[input$behaviorsDT_select,]

      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowBehav)
      dupRow$behavior_time <- paste(dupRow$behavior_time, "EDIT !")
      dupRow$actor <- "ENTER ACTOR"
      dupRow$subject <- "ENTER SUBJECT"

      tableValues$dataOutput$behaviorsTable <- smartbind(tableValues$dataOutput$behaviorsTable, dupRow)
      
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
 
 	observeEvent(input$duplicateScanListRow, {
		if(!is.null(input$scanListDT_select)){
			if(!scanListRV()$scan_time[input$scanListDT_select]==""){
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[input$sessionsDT_select,]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowScanList <- scanListRV()[input$scanListDT_select,]

      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList)
      dupRow$scan_time <- paste(dupRow$scan_time, "EDIT !")

      tableValues$dataOutput$scansTable <- smartbind(tableValues$dataOutput$scansTable, dupRow)
      
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
  
  	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(emptyScanRow(),
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
    

 	observeEvent(input$duplicateScanRow, {
    		
		if(!is.null(input$scansDT_select)){
			if(!scansRV()$scanned_individual_ID[input$scansDT_select]=="") {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[input$sessionsDT_select,]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowScanList <- scanListRV()[input$scanListDT_select,]
      dupRowScan <- scansRV()[input$scansDT_select,]

      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList, dupRowScan)
      dupRow$scanned_individual_ID <- paste(dupRow$scanned_individual_ID, "EDIT !")

      tableValues$dataOutput$scansTable <- smartbind(tableValues$dataOutput$scansTable, dupRow)
  	  tableValues$dataOutput$scansTable <- tableValues$dataOutput$scansTable[!duplicated(tableValues$dataOutput$scansTable),]
  
  
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

