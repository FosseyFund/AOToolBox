	observeEvent(input$deleteSessionRow, {
		if(!is.null(input$sessionsDT_select)) {
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_time[input$sessionsDT_select]
      cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2, " nb rows to delete = ", sum(views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2), "\n"))
      views$dat1 <- views$dat1[!(views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2),]
      views$dat2 <- views$dat2[!(views$dat2$device_id==pk1 & views$dat2$session_start_time==pk2),]
      #clicked$deleteSession <- !clicked$deleteSession
      #output$focalDT <- NULL
      
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
    
    	observeEvent(input$deleteFocalRow, {
    	cat(file=stderr(), paste0("deleting focal row : ", input$focalsDT_select, "\n"))
    		
		if(!is.null(input$focalsDT_select)) {
			
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- focalsRV()$focal_start_time[input$focalsDT_select]
      pk3 <- sessionsRV()$session_start_time[input$sessionsDT_select]
      cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2, " nb rows to delete = ",sum(views$dat1$device_id==pk1 & views$dat1$focal_start_time==pk2), "\n"))


if(length(unique(views$dat1$focal_start_time[views$dat1$device_id==pk1 & views$dat1$session_start_time ==pk3]))==1) {
      	cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2, " only one focal row left", "\n"))
      	views$dat1[views$dat1$device_id==pk1 & views$dat1$focal_start_time==pk2, -c(1:5, (ncol(views$dat1)-7):(ncol(views$dat1)-2))] <- NA
     	views$dat1 <- views$dat1[!duplicated(views$dat1),]

		cat(file=stderr(), paste0("nrow scan to delete = ", sum(views$dat2$device_id==pk1 & views$dat2$focal_start_time==pk2), "\n"))
		
      	views$dat2[views$dat2$device_id==pk1 & views$dat2$focal_start_time==pk2, -c(1:5, (ncol(views$dat2)-7):(ncol(views$dat2)-2))] <- NA
      	views$dat2 <- views$dat2[!duplicated(views$dat2),]

      } else {


       views$dat1 <- views$dat1[!(views$dat1$device_id==pk1 & views$dat1$focal_start_time==pk2),]
       cat(file=stderr(), paste0("nrow scan to delete = ", sum(views$dat2$device_id==pk1 & views$dat2$focal_start_time==pk2), "\n"))
       views$dat2 <- views$dat2[!(views$dat2$device_id==pk1 & views$dat2$focal_start_time==pk2),]
      }
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
    
    
    observeEvent(input$deleteBehaviorRow, {
    	    	cat(file=stderr(), paste0("deleting behavior row : ", input$behaviorsDT_select, "\n"))
	if(!is.null(input$behaviorsDT_select)) {
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]		
      pk2 <- behaviorsRV()$behavior_time[input$behaviorsDT_select]
      pk3 <- behaviorsRV()$actor[input$behaviorsDT_select]
      pk4 <- behaviorsRV()$subject[input$behaviorsDT_select]
      pk5 <- focalsRV()$focal_start_time[input$focalsDT_select]
      if(sum(views$dat1$device_id==pk1 & views$dat1$focal_start_time==pk5)==1) {
      	views$dat1[views$dat1$device_id==pk1 & views$dat1$focal_start_time==pk5, -c(1:8, (ncol(views$dat1)-7):ncol(views$dat1))] <- NA
      } else {
       views$dat1 <- views$dat1[!(views$dat1$device_id==pk1 & views$dat1$behavior_time==pk2 & views$dat1$actor==pk3 & views$dat1$subject==pk4),]
       }
       ###EXCEPT IF ONLY ONE ROW FOR THE FOCAL
      #cat(file=stderr(), paste0("set row class for focalsDT row : ", focalsDTSelectedRow, "\n"))

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
	
	
	
	    	observeEvent(input$deleteScanListRow, {
    	cat(file=stderr(), paste0("deleting scanListDT row : ", input$scanListDT_select, "\n"))
    		
		if(!is.null(input$scanListDT_select)) {
			
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_time[input$scanListDT_select]
      
      cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2, " nb rows to delete = ",sum(views$dat2$device_id==pk1 & views$dat2$scan_time ==pk2 & !is.na(views$dat2$scan_time)), "\n"))

      pk3 <- focalsRV()$focal_start_time[input$focalsDT_select]
      if(length(unique(views$dat2$scan_time[views$dat2$device_id==pk1 & views$dat2$focal_start_time==pk3]))==1) {
      	cat(file=stderr(), paste0("pk1 = ", pk1," pk3 = ", pk3, " only one scan row left", "\n"))
      	views$dat2[views$dat2$device_id==pk1 & views$dat2$focal_start_time==pk3, -c(1:8, (ncol(views$dat2)-7):ncol(views$dat2))] <- NA
      	views$dat2 <- views$dat2[!duplicated(views$dat2),]

      } else {
       views$dat2 <- views$dat2[!(views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & !is.na(views$dat2$scan_time)),]      
      }
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

	    	observeEvent(input$deleteScanRow, {
    	cat(file=stderr(), paste0("deleting scansDT row : ", input$scansDT_select, "\n"))
    		
		if(!is.null(input$scansDT_select)) {
			
 	  pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_time[input$scanListDT_select]
      pk3 <- scansRV()$scanned_individual_id[input$scansDT_select]
            
      cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2," pk3 = ", pk3, " nb rows to delete = ", sum(views$dat2$device_id==pk1 & views$dat2$scan_time ==pk2 & views$dat2$scanned_individual_id ==pk3 & !is.na(views$dat2$scan_time)), "\n"))

     if(sum(views$dat2$device_id==pk1 & views$dat2$scan_time ==pk2 & !is.na(views$dat2$scan_time))==1) {
      	cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2, " only one scan row left", "\n"))
      	views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & !is.na(views$dat2$scan_time), -c(1:9, (ncol(views$dat2)-11):ncol(views$dat2))] <- NA
      } else {
       views$dat2 <- views$dat2[!(views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & views$dat2$scanned_individual_id==pk3 & !is.na(views$dat2$scan_time)),]      
      }
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

