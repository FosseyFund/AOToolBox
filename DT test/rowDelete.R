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
				     cat(file=stderr(), paste0("render focalsDTDelete", "\n"))
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
      col_types = rep("string", ncol(isolate(emptyScanListRow()))));
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
      col_types = rep("string", ncol(isolate(emptyScanRow()))));
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


##########################
##########################
##########################

observeEvent(input$deleteDayVarsRow, {
    	    	cat(file=stderr(), paste0("deleting dayVars row : ", input$dayVarsDT_select, "\n"))
	if(!is.null(input$dayVarsDT_select)) {
		
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- dayVarsRV()$dayVars[input$dayVarsDT_select]

      tableValues$dayVarsTable <- tableValues$dayVarsTable[!(tableValues$dayVarsTable$device_ID==pk1 & tableValues$dayVarsTable$session_start_timeStamp==pk2 & tableValues$dayVarsTable$dayVars==pk3),]

		    
   output$dayVarsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render dayVarsDTDelete", "\n"))

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
	})

##########################
##########################
##########################

observeEvent(input$deleteCommentsRow, {
    	    	cat(file=stderr(), paste0("deleting comment row : ", input$commentsDT_select, "\n"))
	if(!is.null(input$commentsDT_select)) {
		
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk4 <- commentsRV()$comment_timeStamp[input$commentsDT_select]

      tableValues$commentsTable <- tableValues$commentsTable[!(tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable $session_start_timeStamp==pk2 & tableValues$commentsTable$focal_start_timeStamp ==pk3 & tableValues$commentsTable$comment_timeStamp ==pk4),]

		    
   output$commentsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render commentsDTDelete", "\n"))
  	if(is.null(isolate(emptyCommentRow()))) return(NULL)

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
	})
	
##########################
##########################
##########################

observeEvent(input$deleteBackgroundTapsRow, {
    	    	cat(file=stderr(), paste0("deleting backgroundTaps row : ", input$backgroundTapsDT_select, "\n"))
	if(!is.null(input$backgroundTapsDT_select)) {
		
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk4 <- backgroundTapsRV()$backgroundTap_timeStamp[input$backgroundTapsDT_select]

      tableValues$backgroundTapsTable <- tableValues$backgroundTapsTable[!(tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$session_start_timeStamp==pk2 & tableValues$backgroundTapsTable$focal_start_timeStamp ==pk3 & tableValues$backgroundTapsTable$backgroundTap_timeStamp==pk4),]

		    
output$backgroundTapsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render backgroundTapsDTDelete", "\n"))
  	if(is.null(isolate(emptyBackgroundTapsRow()))) return(NULL)

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
	})
	
##########################
##########################
##########################

observeEvent(input$deleteFocalVarsRow, {
    	    	cat(file=stderr(), paste0("deleting focalVars row : ", input$focalVarsDT_select, "\n"))
	if(!is.null(input$focalVarsDT_select)) {
		
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk4 <- focalVarsRV()$focalVars[input$focalVarsDT_select]

      tableValues$focalVarsTable <- tableValues$focalVarsTable[!(tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$session_start_timeStamp==pk2 & tableValues$focalVarsTable$focal_start_timeStamp ==pk3 & tableValues$focalVarsTable$focalVars ==pk4),]

		    
output$focalVarsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render focalVarsDTDelete", "\n"))
  	if(is.null(isolate(emptyFocalVarsRow()))) return(NULL)

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
	})
	
##########################
##########################
##########################

observeEvent(input$deleteContinuousVarsRow, {
    	    	cat(file=stderr(), paste0("deleting ContinuousVars row : ", input$continuousVarsDT_select, "\n"))
	if(!is.null(input$continuousVarsDT_select)) {
		
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk4 <- continuousVarsRV()$continuousVars[input$continuousVarsDT_select]

      tableValues$continuousVarsTable <- tableValues$continuousVarsTable[!(tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$session_start_timeStamp==pk2 & tableValues$continuousVarsTable$focal_start_timeStamp ==pk3 & tableValues$continuousVarsTable$continuousVars ==pk4),]

		    
output$continuousVarsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render continuousVarsDTDelete", "\n"))
  	if(is.null(isolate(emptyContinuousVarsRow()))) return(NULL)

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
	})

##########################
##########################
##########################

observeEvent(input$deleteScanVarsRow, {
    	    	cat(file=stderr(), paste0("deleting ScanVars row : ", input$scanVarsDT_select, "\n"))
	if(!is.null(input$scanVarsDT_select)) {
		
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk4 <- scanListRV()$scan_timeStamp[input$scanListDT_select]
      pk5 <- scanVarsRV()$scanVars[input$scanVarsDT_select]

      tableValues$scanVarsTable <- tableValues$scanVarsTable[!(tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$session_start_timeStamp==pk2 & tableValues$scanVarsTable$focal_start_timeStamp ==pk3 & tableValues$scanVarsTable$scan_timeStamp ==pk4 & tableValues$scanVarsTable$scanVars ==pk5),]

		    
output$scanVarsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render scanVarsDTDelete", "\n"))
  	if(is.null(isolate(emptyScanVarsRow()))) return(NULL)

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
	})

