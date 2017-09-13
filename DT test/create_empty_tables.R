emptySessionRow <- function(){
		if(is.null(tableValues$dataOutput$sessionsTable)) return(data.frame(empty="", stringsAsFactors=F))
    	sessionColnames <- names(tableValues$dataOutput$sessionsTable)
    	dat <- data.frame(matrix(NA,nrow=1, ncol=length(sessionColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- sessionColnames
    	return(dat)
    	}
    	
emptyFocalListRow <- function(){
		if(is.null(tableValues$dataOutput$focalsTable)) return(data.frame(empty="", stringsAsFactors=F))

    	focalListColnames <- names(tableValues$dataOutput$focalsTable)
    	cat(file=stderr(), paste0("focalListColnames = ", focalListColnames,"\n"))

    	focalListColnames <- focalListColnames[,c(length(focalListColnames), 3:(length(focalListColnames)-1))]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(focalListColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- focalListColnames
    	return(dat)
    	}
    	
emptyBehaviorRow <- function(){
		if(is.null(tableValues$dataOutput$behaviorsTable)) return(data.frame(empty="", stringsAsFactors=F))

    	behaviorColnames <- names(tableValues$dataOutput$behaviorsTable)
    	behaviorColnames <- behaviorColnames[!behaviorColnames%in%c("device_ID","session_start_timeStamp","focal_start_timeStamp")]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(behaviorColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- behaviorColnames
    	return(dat)
    	}
    	
emptyScanListRow <- function(){
				if(is.null(tableValues$dataOutput$scansTable)) return(data.frame(empty="", stringsAsFactors=F))

		scanListColnames <- names(tableValues$dataOutput$scansTable)[c(4,(length(names(tableValues$dataOutput$scansTable))-4):(length(names(tableValues$dataOutput$scansTable))))]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(scanListColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- scanListColnames
    	return(dat)
    	}

emptyScanRow <- function(){
					if(is.null(tableValues$dataOutput$scansTable)) return(data.frame(empty="", stringsAsFactors=F))

		scanColnames <- names(tableValues$dataOutput$scansTable)[5:(length(names(tableValues$dataOutput$scansTable))-6)]
    	dat <- data.frame(matrix("",nrow=1, ncol=length(scanColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- scanColnames
    	return(dat)
    	}
    	
    	
    	
    	
    	