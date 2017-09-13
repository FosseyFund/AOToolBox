sessionsRV <-  reactive({
    	if (is.null(tableValues$dataOutput$sessionsTable)) return(emptySessionRow())
    	if (nrow(tableValues$dataOutput$sessionsTable)==0) return(emptySessionRow())

    	res <- tableValues$dataOutput$sessionsTable
    	cat(file=stderr(), paste0("sessionsRV updated : ", nrow(res), "\n"))
	    return(res)
})


focalsRV <- reactive({
		if(isolate(is.null(input$sessionsDT_select))) return(emptyFocalListRow())##checks if a sessionsDT row has been selected
		res <-tableValues$dataOutput$focalsTable[tableValues$dataOutput$focalsTable$device_ID==sessionsRV()$device_ID[input$sessionsDT_select] & tableValues$dataOutput$focalsTable$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select],]
		res <- res[,names(res)%in%names(emptyFocalListRow())]
		cat(file=stderr(), paste0("focalsRV updated with sessionsDT_select = ",isolate(input$sessionsDT_select)," and ", res[1,1], " and input$focalsDT_select = ", isolate(input$focalsDT_select),"\n\n"))
		return(res)
})
	

behaviorsRV <- reactive({
		if((is.null(input$sessionsDT_select) | is.null(input$focalsDT_select))) return(emptyBehaviorRow())

		cat(file=stderr(), paste0("behaviorsRV updated with sessionsDT_select = ",input$sessionsDT_select," and input$focalsDT_select = ", input$focalsDT_select, "\n\n"))
		res <- tableValues$dataOutput$behaviorsTable[tableValues$dataOutput$behaviorsTable$device_ID==sessionsRV()$device_ID[input$sessionsDT_select] & tableValues$dataOutput$behaviorsTable$focal_start_timeStamp==focalsRV()$focal_start_timeStamp[input$focalsDT_select] & tableValues$dataOutput$behaviorsTable$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select],]
		res <- res[,names(res)%in%names(emptyBehaviorRow())]
		return(res)
})
	

scanListRV <- reactive({
		if((is.null(input$sessionsDT_select) | is.null(input$focalsDT_select))) return(emptyScanListRow())

		res <- tableValues$dataOutput$scansTable[tableValues$dataOutput$scansTable$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & tableValues$dataOutput$scansTable$session_start_time==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & tableValues$dataOutput$scansTable$focal_start_timeStamp==focalsRV()$focal_start_timeStamp[isolate(input$focalsDT_select)],]
		res <- res[,names(res)%in%names(emptyScanListRow())]
		res <- res[!duplicated(res),]
		return(res)
})


scansRV <- reactive({
		if((is.null(input$scanListDT_select))) return(emptyScanRow())
		#temp <- is.null(input$scansDT_edit)
		
		res <-tableValues$dataOutput$scansTable[tableValues$dataOutput$scansTable$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & tableValues$dataOutput$scansTable$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & tableValues$dataOutput$scansTable$scan_timeStamp==scanListRV()$scan_timeStamp[isolate(input$scanListDT_select)] & !is.na(tableValues$dataOutput$scansTable$scan_timeStamp),]
		res <- res[,names(res)%in%names(emptyScanRow())]
		return(res)
})




