emptySessionRow <- function(){
    	sessionColnames <- c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "layout_info_json_version", "behaviors_json_version", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold")
    	dat <- data.frame(matrix(NA,nrow=1, ncol=length(sessionColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- sessionColnames
    	return(dat)
    	}
    	
    	emptyFocalListRow <- function(){
    focalListColnames <- c("focal_start_time", "focal_end_time", "focal_individual_id", "set_duration", "set_scan_interval")
    	dat <- data.frame(matrix("",nrow=1, ncol=length(focalListColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- focalListColnames
    	return(dat)
    	}
    	
    	
    emptyBehaviorRow <- function(){
    behaviorColnames <- c("behavior_time", "actor", "subject", names(views$dat1)[!names(views$dat1)%in%c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "focal_start_time", "focal_end_time","focal_individual_id", "behavior_time", "actor","subject","gps_on", "compass_on", "map_mode_on", "physical_contact_threshold","layout_info_json_version" , "behaviors_json_version", "set_duration", "set_scan_interval")])
    	dat <- data.frame(matrix("",nrow=1, ncol=length(behaviorColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- behaviorColnames
    	return(dat)
    	}
    	
    	emptyScanListRow <- function(){
    scanListColnames <- c("scan_time", "latitude", "longitude", "gps_horizontal_precision", "altitude")
    	dat <- data.frame(matrix("",nrow=1, ncol=length(scanListColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- scanListColnames
    	return(dat)
    	}


    emptyScanRow <- function(){
    scanColnames <- c("scanned_individual_id", names(views$dat2)[!names(views$dat2)%in%c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "focal_start_time", "focal_end_time","focal_individual_id", "scan_time", "scanned_individual_id","scan_time", "latitude", "longitude", "gps_horizontal_precision", "altitude", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold","layout_info_json_version" , "behaviors_json_version", "set_duration", "set_scan_interval")])
    	dat <- data.frame(matrix("",nrow=1, ncol=length(scanColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- scanColnames
    	return(dat)
    	}