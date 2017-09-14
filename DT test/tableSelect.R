observeEvent(input$sessionsDT_select, {
		     cat(file=stderr(), paste0("is.null(input$sessionsDT_select) = ", is.null(input$sessionsDT_select), "\n"))
	output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDT", "\n"))
				     cat(file=stderr(), paste0("ncol(emptyFocalListRow()) = ", ncol(isolate(emptyFocalListRow())),"\n"))
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
						     cat(file=stderr(), paste0("render behaviorsDT", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyBehaviorRow())))
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
								     cat(file=stderr(), paste0("render scanListsDT", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyScanListRow())))
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
								     cat(file=stderr(), paste0("render scansDT", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyScanRow())))
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
############################### 
############################### 
############################### 
    	  
	observeEvent(input$focalsDT_select, {
			#if(is.null(input$focalsDT_select)) output$behaviorsDT <- NULL

		output$behaviorsDT <- renderD3tf({
									     cat(file=stderr(), paste0("render behaviorsDTbis", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyBehaviorRow())))
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
  output$scanListDT <- renderD3tf({
  							     cat(file=stderr(), paste0("render scanListDTbis", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyScanListRow())))
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
  	  							     cat(file=stderr(), paste0("render scansDTbis", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyScanRow())))
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
############################### 
############################### 
############################### 

	observeEvent(input$scanListDT_select, {
	output$scansDT <- renderD3tf({
		  							     cat(file=stderr(), paste0("render scansDTter", "\n"))
cat(file=stderr(), paste0("scansRV dim : ", paste(isolate(dim(scansRV())), collapse=", "), "\n"))
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
	}) 