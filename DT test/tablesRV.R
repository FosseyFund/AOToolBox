sessionsRV <-  function(){
    	if (is.null(tableValues$sessionsTable)) return(emptySessionRow())
    	if (nrow(tableValues$sessionsTable)==0) return(emptySessionRow())

    	res <- tableValues$sessionsTable
	for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
    	cat(file=stderr(), paste0("sessionsRV updated : ", nrow(res), "\n"))
	    return(res)
}

dayVarsRV <- function(){
		if(isolate(is.null(input$sessionsDT_select))) return(emptyDayVarsRow())##checks if a sessionsDT row has been selected
		res <- tableValues$dayVarsTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[input$sessionsDT_select] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select],]
		dayVarsColnames <- names(tableValues$dayVarsTable)
		res <- res[,3:(length(dayVarsColnames))]
		if(nrow(res)==0) res <- emptyDayVarsRow()
		cat(file=stderr(), paste0("focalsRV updated with sessionsDT_select = ",isolate(input$sessionsDT_select)," and ", res[1,1], " and input$focalsDT_select = ", isolate(input$focalsDT_select),"\n\n"))
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

commentsRV <- function(){
		if((is.null(input$sessionsDT_select) | is.null(input$focalsDT_select))) return(emptyCommentRow())
		cat(file=stderr(), paste0("commentsRV about to be created\n"))
		res <- tableValues$commentsTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & res$focal_start_timeStamp==focalsRV()$focal_start_timeStamp[isolate(input$focalsDT_select)],]
		cat(file=stderr(), paste0("commentsRV created\n"))
		res <- res[,4:length(names(res))]
		res <- res[!duplicated(res),]
		if(nrow(res)==0) res <- emptyCommentRow()
		return(res)
}

backgroundTapsRV <- function(){
		if((is.null(input$sessionsDT_select) | is.null(input$focalsDT_select))) return(emptyBackgroundTapsRow())
		cat(file=stderr(), paste0("backgroundTapsRV about to be created\n"))
		res <- tableValues$backgroundTapsTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & res$focal_start_timeStamp==focalsRV()$focal_start_timeStamp[isolate(input$focalsDT_select)],]
		cat(file=stderr(), paste0("backgroundTapsRV created\n"))
		res <- res[,4:length(names(res))]
		res <- res[!duplicated(res),]
		if(nrow(res)==0) res <- emptyBackgroundTapsRow()
		return(res)
}

focalVarsRV <- function(){
		if((is.null(input$sessionsDT_select) | is.null(input$focalsDT_select))) return(emptyFocalVarsRow())
		cat(file=stderr(), paste0("focalVarsRV about to be created\n"))
		res <- tableValues$focalVarsTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & res$focal_start_timeStamp==focalsRV()$focal_start_timeStamp[isolate(input$focalsDT_select)],]
		cat(file=stderr(), paste0("focalVarsRV created\n"))
		res <- res[,4:length(names(res))]
		res <- res[!duplicated(res),]
		if(nrow(res)==0) res <- emptyFocalVarsRow()
		return(res)
}

continuousVarsRV <- function(){
		if(is.null(input$sessionsDT_select) | is.null(input$focalsDT_select) | is.null(tableValues$continuousVarsTable) | ncol(tableValues$continuousVarsTable)==3 ) return(emptyContinuousVarsRow())
		cat(file=stderr(), paste0("continuousVarsRV about to be created\n"))
		res <- tableValues$continuousVarsTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & res$focal_start_timeStamp==focalsRV()$focal_start_timeStamp[isolate(input$focalsDT_select)],]
		cat(file=stderr(), paste0("continuousVarsRV created\n"))
		res <- res[,4:length(names(res))]
		res <- res[!duplicated(res),]
		if(nrow(res)==0) res <- emptyContinuousVarsRow()
		return(res)
}

scanListRV <- function(){
		if((is.null(input$sessionsDT_select) | is.null(input$focalsDT_select))) return(emptyScanListRow())
		cat(file=stderr(), paste0("scanListRV about to be created\n"))
		res <- tableValues$scansTable
				cat(file=stderr(), paste0("nrow1 (scanListRV) = ",nrow(res),"\n"))

		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & res$focal_start_timeStamp==focalsRV()$focal_start_timeStamp[isolate(input$focalsDT_select)],]
		cat(file=stderr(), paste0("scanListRV created\n"))
		cat(file=stderr(), paste0("nrow2 (scanListRV) = ",nrow(res),"\n"))
		res <- res[,c(4,(length(names(res))-4):(length(names(res))))]
		res <- res[!duplicated(res),]
		cat(file=stderr(), paste0("nrow3 (scanListRV) = ",nrow(res),"\n"))
		if(nrow(res)==0) res <- emptyScanListRow()
		return(res)
}

scanVarsRV <- function(){
		if((is.null(input$scanListDT_select))) return(emptyScanVarsRow())
		#temp <- is.null(input$scansDT_edit)
		res <-tableValues$scanVarsTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & res$scan_timeStamp==scanListRV()$scan_timeStamp[isolate(input$scanListDT_select)] & !is.na(res$scan_timeStamp),]
		res <- res[,5:length(names(res))]
		if(nrow(res)==0) res <- emptyScanVarsRow()
		return(res)
}


scansRV <- function(){
		if((is.null(input$scanListDT_select))) return(emptyScanRow())
		#temp <- is.null(input$scansDT_edit)
		res <-tableValues$scansTable
		for (i in 1:ncol(res)) res[,i] <- as.character(res[,i])
		res <- res[res$device_ID==sessionsRV()$device_ID[isolate(input$sessionsDT_select)] & res$session_start_timeStamp==sessionsRV()$session_start_timeStamp[input$sessionsDT_select] & res$scan_timeStamp==scanListRV()$scan_timeStamp[isolate(input$scanListDT_select)] & !is.na(res$scan_timeStamp),]
		res <- res[,5:(length(names(res))-6)]
		if(nrow(res)==0 | (nrow(res)==1 & sum(is.na(res$scanned_individual_ID))==1)) res <- emptyScanRow()
		return(res)
}




