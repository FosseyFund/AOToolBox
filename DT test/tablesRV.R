sessionsRV <-  function(){
    	if (is.null(tableValues$sessionsTable)) return(emptySessionRow())
    	if (nrow(tableValues$sessionsTable)==0) return(emptySessionRow())

    	res <- tableValues$sessionsTable
	for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
    	cat(file=stderr(), paste0("sessionsRV updated : ", nrow(res), "\n"))
	    return(res)
}


focalsRV <- function(){
		if(isolate(is.null(input$sessionsDT_select))) return(emptyFocalListRow())##checks if a sessionsDT row has been selected
		res <- tableValues$focalsTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[input$sessionsDT_select] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select],]
		focalListColnames <- names(tableValues$focalsTable)
		res <- res[,c(length(focalListColnames), 3:(length(focalListColnames)-1))]
		if(nrow(res)==0) res <- emptyFocalListRow()
		cat(file=stderr(), paste0("focalsRV updated with sessionsDT_select = ",isolate(input$sessionsDT_select)," and ", res[1,1], " and input$focalsDT_select = ", isolate(input$focalsDT_select),"\n\n"))
		return(res)
}
	

behaviorsRV <- function(){
		if((is.null(input$sessionsDT_select) | is.null(input$focalsDT_select))) return(emptyBehaviorRow())

		cat(file=stderr(), paste0("behaviorsRV updated with sessionsDT_select = ",input$sessionsDT_select," and input$focalsDT_select = ", input$focalsDT_select, "\n\n"))
		res <- tableValues$behaviorsTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[input$sessionsDT_select] & res$focal_start_timeStamp==focalsRV()$focal_start_timeStamp[input$focalsDT_select] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select],]
		behaviorColnames <- names(res)
		res <- res[,!behaviorColnames%in%c("device_ID","session_start_timeStamp","focal_start_timeStamp")]
		if(nrow(res)==0) res <- emptyBehaviorRow()
		return(res)
}
	

scanListRV <- function(){
		if((is.null(input$sessionsDT_select) | is.null(input$focalsDT_select))) return(emptyScanListRow())
		cat(file=stderr(), paste0("scanListRV about to be created\n"))
		res <- tableValues$scansTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & res$focal_start_timeStamp==focalsRV()$focal_start_timeStamp[isolate(input$focalsDT_select)],]
		cat(file=stderr(), paste0("scanListRV created\n"))
		res <- res[,c(4,(length(names(res))-4):(length(names(res))))]
		res <- res[!duplicated(res),]
		if(nrow(res)==0) res <- emptyScanListRow()
		return(res)
}


scansRV <- function(){
		if((is.null(input$scanListDT_select))) return(emptyScanRow())
		#temp <- is.null(input$scansDT_edit)
		res <-tableValues$scansTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & res$scan_timeStamp==scanListRV()$scan_timeStamp[isolate(input$scanListDT_select)] & !is.na(res$scan_timeStamp),]
		res <- res[,5:(length(names(res))-6)]
		if(nrow(res)==0) res <- emptyScanRow()
		return(res)
}




