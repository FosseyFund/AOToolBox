emptySessionRow <- function(){
		if(is.null(tableValues$sessionsTable)) return(data.frame(empty="", stringsAsFactors=F))
    	sessionColnames <- names(tableValues$sessionsTable)
    	dat <- data.frame(matrix(NA,nrow=1, ncol=length(sessionColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- sessionColnames
    	return(dat)
    	}

emptyDayVarsRow <- function(){
		if(is.null(tableValues$dayVarsTable)) return(NULL)
		
    	dayVarsColnames <- names(tableValues$dayVarsTable)
    	dayVarsColnames <- dayVarsColnames[3:length(dayVarsColnames)]
    	dat <- data.frame(matrix(NA,nrow=1, ncol=length(dayVarsColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- dayVarsColnames
    	return(dat)
    	}
    	
emptyFocalListRow <- function(){
		if(is.null(tableValues$focalsTable)) return(NULL)

    	focalListColnames <- names(tableValues$focalsTable)
    	focalListColnames <- focalListColnames[c(length(focalListColnames), 3:(length(focalListColnames)-1))]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(focalListColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- focalListColnames
    	return(dat)
    	}
    	
emptyBehaviorRow <- function(){
		if(is.null(tableValues$behaviorsTable)) return(data.frame(empty="", stringsAsFactors=F))

    	behaviorColnames <- names(tableValues$behaviorsTable)
    	behaviorColnames <- behaviorColnames[!behaviorColnames%in%c("device_ID","session_start_timeStamp","focal_start_timeStamp")]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(behaviorColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- behaviorColnames
    	return(dat)
    	}
 
emptyCommentRow <- function(){
		if(is.null(tableValues$commentsTable) | ncol(tableValues$commentsTable)==3) return(NULL)

    	commentsColnames <- names(tableValues$commentsTable)
    	commentsColnames <- commentsColnames[4:length(commentsColnames)]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(commentsColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- commentsColnames
    	return(dat)
    	}
    	
emptyBackgroundTapsRow <- function(){
		if(is.null(tableValues$backgroundTapsTable)) return(data.frame(empty="", stringsAsFactors=F))

    	backgroundTapsColnames <- names(tableValues$backgroundTapsTable)
    	backgroundTapsColnames <- backgroundTapsColnames[4:length(backgroundTapsColnames)]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(backgroundTapsColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- backgroundTapsColnames
    	return(dat)
    	}

emptyFocalVarsRow <- function(){
		if(is.null(tableValues$focalVarsTable)) return(data.frame(empty="", stringsAsFactors=F))

    	focalVarsColnames <- names(tableValues$focalVarsTable)
    	focalVarsColnames <- focalVarsColnames[4:length(focalVarsColnames)]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(focalVarsColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- focalVarsColnames
    	return(dat)
    	}

emptyContinuousVarsRow <- function(){
		if(is.null(tableValues$continuousVarsTable) | ncol(tableValues$continuousVarsTable)==3 ) return(NULL)

    	continuousVarsColnames <- names(tableValues$continuousVarsTable)
    	continuousVarsColnames <- continuousVarsColnames[4:length(continuousVarsColnames)]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(continuousVarsColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- continuousVarsColnames
    	return(dat)
    	}  	 
    	   	
emptyScanListRow <- function(){
				if(is.null(tableValues$scansTable)) return(data.frame(empty="", stringsAsFactors=F))

		scanListColnames <- names(tableValues$scansTable)[c(4,(length(names(tableValues$scansTable))-4):(length(names(tableValues$scansTable))))]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(scanListColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- scanListColnames
    	return(dat)
    	}

emptyScanVarsRow <- function(){
		if(is.null(tableValues$scanVarsTable) | ncol(tableValues$scanVarsTable)==4 ) return(NULL)

    	scanVarsColnames <- names(tableValues$scanVarsTable)
    	 #cat(file=stderr(), paste0("scanVarsColnames1 = ", paste(scanVarsColnames, collapse=";"),"\n"))
    	scanVarsColnames <- scanVarsColnames[5:length(scanVarsColnames)]
    	 #cat(file=stderr(), paste0("scanVarsColnames2 = ", paste(scanVarsColnames, collapse=";"),"\n"))
    	dat <- data.frame(matrix("",nrow=1, ncol=length(scanVarsColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- scanVarsColnames
    	return(dat)
    	}
    	
emptyScanRow <- function(){
					if(is.null(tableValues$scansTable)) return(data.frame(empty="", stringsAsFactors=F))

		scanColnames <- names(tableValues$scansTable)[5:(length(names(tableValues$scansTable))-6)]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(scanColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- scanColnames
    	return(dat)
    	}
    	
    	
    	
    	
    	