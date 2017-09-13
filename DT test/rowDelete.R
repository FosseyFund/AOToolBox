observeEvent(input$deleteSessionRow, {
		if(!is.null(input$sessionsDT_select) & nrow(sessionsRV())>1) {
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      tableValues$sessionsTable <- tableValues$sessionsTable[!(tableValues$sessionsTable$device_ID==pk1 & tableValues$sessionsTable$session_start_timeStamp==pk2),]
      tableValues$focalsTable <- tableValues$focalsTable[!(tableValues$focalsTable$device_ID==pk1 & tableValues$focalsTable$session_start_timeStamp==pk2),]
      tableValues$behaviorsTable <- tableValues$behaviorsTable[!(tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$session_start_timeStamp==pk2),]
      tableValues$scansTable <- tableValues$scansTable[!(tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$session_start_timeStamp==pk2),]     
      tableValues$backgroundTapsTable <- tableValues$backgroundTapsTable[!(tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$session_start_timeStamp==pk2),] 
      tableValues$commentsTable <- tableValues$commentsTable[!(tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$session_start_timeStamp==pk2),]
      tableValues$continuousVarsTable <- tableValues$continuousVarsTable[!(tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$session_start_timeStamp==pk2),]
      tableValues$dayVarsTable <- tableValues$dayVarsTable[!(tableValues$dayVarsTable$device_ID==pk1 & tableValues$dayVarsTable$session_start_timeStamp==pk2),]
      tableValues$focalVarsTable <- tableValues$focalVarsTable[!(tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$session_start_timeStamp==pk2),]
      tableValues$scanVarsTable <- tableValues$scanVarsTable[!(tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$session_start_timeStamp==pk2),]

                      
      output$sessionsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render sessionsDTDelete", "\n"))

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
				     cat(file=stderr(), paste0("render focalsDTDelete", "\n"))
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
						     cat(file=stderr(), paste0("render behaviorsDTDelete", "\n"))

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
		
	output$scanListDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scanListsDTDelete", "\n"))

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

	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDelete", "\n"))

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
	})   
################################  
################################  
################################  

observeEvent(input$deleteFocalRow, {
    	cat(file=stderr(), paste0("deleting focal row : ", input$focalsDT_select, "\n"))
    		
		if(!is.null(input$focalsDT_select)) {
			
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk3 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
 
tableValues$focalsTable <- tableValues$focalsTable[!(tableValues$focalsTable$device_ID==pk1 & tableValues$focalsTable$focal_start_timeStamp==pk2),]

tableValues$behaviorsTable <- tableValues$behaviorsTable[!(tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$focal_start_timeStamp==pk2),]
 
tableValues$scansTable <- tableValues$scansTable[!(tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$focal_start_timeStamp==pk2),]
 
tableValues$backgroundTapsTable <- tableValues$backgroundTapsTable[!(tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$focal_start_timeStamp==pk2),]
  
tableValues$commentsTable <- tableValues$commentsTable[!(tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$focal_start_timeStamp==pk2),]
  
tableValues$continuousVarsTable <- tableValues$continuousVarsTable[!(tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$focal_start_timeStamp==pk2),]
        
tableValues$focalVarsTable <- tableValues$focalVarsTable[!(tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$focal_start_timeStamp==pk2),]
    
tableValues$scanVarsTable <- tableValues$scanVarsTable[!(tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$focal_start_timeStamp==pk2),]
    
               
	output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDTDelete", "\n"))
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
						     cat(file=stderr(), paste0("render behaviorsDTDelete", "\n"))

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
								     cat(file=stderr(), paste0("render scanListsDTDelete", "\n"))

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
								     cat(file=stderr(), paste0("render scansDTDelete", "\n"))
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
	})  
    
#############################    
#############################    
#############################    

observeEvent(input$deleteBehaviorRow, {
    	    	cat(file=stderr(), paste0("deleting behavior row : ", input$behaviorsDT_select, "\n"))
	if(!is.null(input$behaviorsDT_select)) {
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]		
      pk2 <- behaviorsRV()$behavior_timeStamp[input$behaviorsDT_select]
      pk3 <- behaviorsRV()$actor[input$behaviorsDT_select]
      pk4 <- behaviorsRV()$subject[input$behaviorsDT_select]
      pk5 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
 
tableValues$behaviorsTable <- tableValues$behaviorsTable[!(tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$behavior_timeStamp==pk2 & tableValues$behaviorsTable$actor==pk3 & tableValues$behaviorsTable$subject==pk4),]
 
     
   output$behaviorsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render behaviorsDTDelete", "\n"))

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
	})
	
#####################################	
#####################################	
#####################################	
	
observeEvent(input$deleteScanListRow, {
    	cat(file=stderr(), paste0("deleting scanListDT row : ", input$scanListDT_select, "\n"))
    		
	if(!is.null(input$scanListDT_select)) {
			
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_timeStamp[input$scanListDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]

tableValues$scansTable <- tableValues$scansTable[!(tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$scan_timeStamp==pk2 & !is.na(tableValues$scansTable$scan_timeStamp)),]
      
    output$scanListDT <- renderD3tf({
				     cat(file=stderr(), paste0("render scanListDTDelete", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyScanListRow())));
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
								     cat(file=stderr(), paste0("render scansDTDelete", "\n"))
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
	})  

######################
######################
######################

observeEvent(input$deleteScanRow, {
    	cat(file=stderr(), paste0("deleting scansDT row : ", input$scansDT_select, "\n"))
    		
		if(!is.null(input$scansDT_select)) {
			
 	  pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_timeStamp[input$scanListDT_select]
      pk3 <- scansRV()$scanned_individual_ID[input$scansDT_select]
            

tableValues$scansTable <- tableValues$scansTable[!(tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$scan_timeStamp==pk2 & tableValues$scansTable$scanned_individual_ID==pk3 & !is.na(tableValues$scansTable$scan_timeStamp)),]      
      
            
    output$scansDT <- renderD3tf({
				     cat(file=stderr(), paste0("render scansDTDelete", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyScanRow())));
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
	})  	  

