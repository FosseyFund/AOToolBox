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


output$dayVarsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render dayVarsDT", "\n"))
				     cat(file=stderr(), paste0("ncol(emptyDayVarsRow()) = ", ncol(isolate(emptyDayVarsRow())),"\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(isolate(emptyDayVarsRow()))));
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
  
output$commentsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render commentsDT", "\n"))

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
								     cat(file=stderr(), paste0("render scanListDT", "\n"))

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

# output$scanVarsDT <- renderD3tf({
		# cat(file=stderr(), paste0("render scanVarsDT", "\n"))

    # tableProps <- list(
      # btn_reset = TRUE,
      # col_types = rep("string", isolate(ncol(isolate(emptyScanVarsRow())))
    # ));
    # d3tf(isolate(emptyScanVarsRow()),
         # tableProps = isolate(tableProps),
         # extensions = list(
           # list(name = "sort")
         # ),
         # showRowNames = FALSE,
         # tableStyle = "table table-bordered",
         # edit = TRUE,
         # selectableRows='single',
         # selectableRowsClass='success'
	# );
  # })


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
  
   output$focalVarsDT <- renderD3tf({
  							     cat(file=stderr(), paste0("render focalVarsDTbis", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyFocalVarsRow())))
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
  
   output$commentsDT <- renderD3tf({
  							     cat(file=stderr(), paste0("render commentsDTbis", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyCommentRow())))
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
  
   output$backgroundTapsDT <- renderD3tf({
  							     cat(file=stderr(), paste0("render backgroundTapsDTbis", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyBackgroundTapsRow())))
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

   output$continuousVarsDT <- renderD3tf({
  							     cat(file=stderr(), paste0("attempting to render continuousVarsDTbis", "\n"))
  	if(is.null(isolate(emptyContinuousVarsRow()))) return(NULL)
  	cat(file=stderr(), paste0("rendering continuousVarsDTbis", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyContinuousVarsRow())))
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

  output$scanVarsDT <- renderD3tf({
  	  							     cat(file=stderr(), paste0("render scanVarsDTbis with ncol = ", ncol(isolate(emptyScanVarsRow())), " and nrow= ",nrow(isolate(emptyScanVarsRow())),"\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(isolate(emptyScanVarsRow())))
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
  
output$scanVarsDT <- renderD3tf({
		  							     cat(file=stderr(), paste0("render scanVarsDTter", "\n"))
cat(file=stderr(), paste0("scanVarsRV dim : ", paste(isolate(dim(scanVarsRV())), collapse=", "), "\n"))
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

	}) 