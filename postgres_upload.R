timeFormat <- function(stamp){
	paste(unlist(strsplit(stamp, ",")), collapse=" ")
}

naFormat <- function(v){
	ifelse(is.na(v),'NULL',v)
}

uploadSessionsTable <- function(sessionsTable, con){
	for (i in 1:nrow(sessionsTable)){
	command <- paste("INSERT INTO main_tables.list_sessions(device_ID, session_start_time, session_end_time, group_ID, pin_code_name, layout_info_json_version, behaviors_json_version, gps_on, compass_on, map_mode_on, physical_contact_threshold)
    SELECT 
    '",as.character(sessionsTable[i,]$device_ID),"',
    '",timeFormat(as.character(sessionsTable[i,]$session_start_timeStamp)),"',
    '",timeFormat(as.character(sessionsTable[i,]$session_end_timeStamp)),"',
    '", as.character(sessionsTable[i,]$group_ID),"',
    '", as.character(sessionsTable[i,]$pin_code_name),"',
    '", as.character(sessionsTable[i,]$layout_info_json_version),"',
    '", as.character(sessionsTable[i,]$behaviors_json_version),"',
    '", as.character(sessionsTable[i,]$gps_on),"',
    '", as.character(sessionsTable[i,]$compass_on),"',
    '", as.character(sessionsTable[i,]$map_mode_on),"',
    '", as.character(sessionsTable[i,]$physical_contact_threshold),"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_sessions WHERE device_ID='",as.character(sessionsTable[i,]$device_ID),"' AND session_start_time='",timeFormat(as.character(sessionsTable[i,]$session_start_timeStamp)),"');", sep="")
	command <- gsub("''", "NULL", command)
	#command <- gsub("NA", "NULL", command)
	command <- gsub("'NULL'", "NULL", command)
	command <- gsub("'NA'", "NULL", command)
	dbGetQuery(con, command)
	}
}

uploadFocalsTable <- function(focalsTable, con){
	for (i in 1:nrow(focalsTable)){
	command <- paste("INSERT INTO main_tables.list_focals(device_ID, session_start_time, focal_start_time, focal_end_time, set_duration, set_scan_interval, focal_individual_ID)
    SELECT 
    '",as.character(focalsTable[i,]$device_ID),"',
    '",timeFormat(as.character(focalsTable[i,]$session_start_timeStamp)),"',
    '",timeFormat(as.character(focalsTable[i,]$focal_start_timeStamp)),"',
    '", as.character(focalsTable[i,]$focal_end_timeStamp),"',
    '", as.character(focalsTable[i,]$focal_set_duration),"',
    '", as.character(focalsTable[i,]$focal_set_scan_interval),"',
    '", as.character(focalsTable[i,]$focal_individual_ID),"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_focals WHERE device_ID='",as.character(focalsTable[i,]$device_ID),"' AND focal_start_time='",timeFormat(as.character(focalsTable[i,]$focal_start_timeStamp)),"');", sep="")
	command <- gsub("''", "NULL", command)
	#command <- gsub("NA", "NULL", command)
	command <- gsub("'NULL'", "NULL", command)
	command <- gsub("'NA'", "NULL", command)	
	dbGetQuery(con, command)
	}
}


uploadBehaviorsTable <- function(behaviorsTable, con){
	temp1 <- names(behaviorsTable)[-c(1:6, (ncol(behaviorsTable)-4):ncol(behaviorsTable))]
	temp2 <- fixHeader(temp1)
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
			tableHeaders <- temp2[-which(nchar(temp1)==regexpr("[*]", temp1))]
		} else {
			tableHeaders <- temp2
		}
	
	for (i in 1:nrow(behaviorsTable)){
		temp3 <- unlist(as.list(behaviorsTable[i,7:(ncol(behaviorsTable)-5)]))[match(tableHeaders, temp2)]
		command <- paste("INSERT INTO main_tables.list_behaviors(device_ID, focal_start_time, behavior_time, actor, subject, ",
	paste(tableHeaders, collapse=", "),
	", comment, latitude, longitude, gps_horizontal_precision, altitude)
    SELECT 
    '",as.character(behaviorsTable[i,]$device_ID),"',
    '",timeFormat(as.character(behaviorsTable[i,]$focal_start_timeStamp)),"',
    '", timeFormat(as.character(behaviorsTable[i,]$behavior_timeStamp)),"',
    '", as.character(behaviorsTable[i,]$actor),"',
    '", as.character(behaviorsTable[i,]$subject),"',
    ", paste0("'",paste(as.character(temp3), collapse="', '"),"'"),",
    '", as.character(behaviorsTable[i,]$comment),"',
    ", naFormat(as.character(behaviorsTable[i,]$latitude)),",
    ", naFormat(as.character(behaviorsTable[i,]$longitude)),",
    ", naFormat(as.character(behaviorsTable[i,]$gps_horizontal_precision)),",
    ", naFormat(as.character(behaviorsTable[i,]$altitude)),"
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_behaviors WHERE device_ID='",as.character(behaviorsTable[i,]$device_ID),"' AND behavior_time='",timeFormat(as.character(behaviorsTable[i,]$behavior_timeStamp)),"' AND actor='",as.character(behaviorsTable[i,]$actor),"' AND	subject='",as.character(behaviorsTable[i,]$subject),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadScansTable <- function(scansTable, con){
	tableHeaders <- fixHeader(names(scansTable))
	
	for (i in 1:nrow(scansTable)){
		command <- paste("INSERT INTO main_tables.list_scans(device_ID, focal_start_time, scan_time, latitude, longitude, gps_horizontal_precision, altitude, compass_bearing)
    SELECT 
    '",as.character(scansTable[i,]$device_ID),"',
    '",timeFormat(as.character(scansTable[i,]$focal_start_timeStamp)),"',
    '", timeFormat(as.character(scansTable[i,]$scan_timeStamp)),"',
    ", naFormat(as.character(scansTable[i,]$latitude)),",
    ", naFormat(as.character(scansTable[i,]$longitude)),",
    ", naFormat(as.character(scansTable[i,]$gps_horizontal_precision)),",
    ", naFormat(as.character(scansTable[i,]$altitude)),",
    ", naFormat(as.character(scansTable[i,]$compass_bearing)),"
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_scans WHERE device_ID='",as.character(scansTable[i,]$device_ID),"' AND scan_time ='",timeFormat(as.character(scansTable[i,]$scan_timeStamp)),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadScanData <- function(scansTable, con){
	temp1 <- names(scansTable)[-c(1:5, (ncol(scansTable)-7):ncol(scansTable))]
	temp2 <- fixHeader(temp1)
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
			tableHeaders <- temp2[-which(nchar(temp1)==regexpr("[*]", temp1))]
		} else {
			tableHeaders <- temp2
		}
	
	for (i in 1:nrow(scansTable)){
		temp3 <- unlist(as.list(scansTable[i,6:(ncol(scansTable)-8)]))[match(tableHeaders, temp2)]
		command <- paste("INSERT INTO main_tables.scan_data(device_ID, scan_time, scanned_individual_ID, ",
	paste(tableHeaders, collapse=", "),
	", x_position, y_position)
    SELECT 
    '",as.character(scansTable[i,]$device_ID),"',
    '", timeFormat(as.character(scansTable[i,]$scan_timeStamp)),"',
    '", as.character(scansTable[i,]$scanned_individual_ID),"',
    ", paste0("'",paste(as.character(temp3), collapse="', '"),"'"),",    
    ", as.character(scansTable[i,]$x_position),",
    ", as.character(scansTable[i,]$y_position),"
	WHERE NOT EXISTS (SELECT 1 from main_tables.scan_data WHERE device_ID='",as.character(scansTable[i,]$device_ID),"' AND scan_time='",timeFormat(as.character(scansTable[i,]$scan_timeStamp)),"' AND scanned_individual_ID ='",as.character(scansTable[i,]$scanned_individual_ID),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}


uploadScanVariables <- function(scanVarsTable, con){
	temp1 <- names(scanVarsTable)[-(1:4)]
	temp2 <- fixHeader(temp1)
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
			tableHeaders <- temp2[-which(nchar(temp1)==regexpr("[*]", temp1))]
		} else {
			tableHeaders <- temp2
		}
	
	for (i in 1:nrow(scanVarsTable)){
		temp3 <- unlist(as.list(scanVarsTable[i,5:ncol(scanVarsTable)]))[match(tableHeaders, temp2)]
		command <- paste("INSERT INTO main_tables.scan_variables(device_ID, scan_time, ",
		paste(tableHeaders, collapse=", "),
	") SELECT 
    '",as.character(scanVarsTable[i,]$device_ID),"',
    '", timeFormat(as.character(scanVarsTable[i,]$scan_timeStamp)),"',
	", paste0("'",paste(as.character(temp3), collapse="', '"),"'")," WHERE NOT EXISTS (SELECT 1 from main_tables.scan_variables WHERE device_ID='",as.character(scanVarsTable[i,]$device_ID),"' AND scan_time='",timeFormat(as.character(scanVarsTable[i,]$scan_timeStamp)),"' AND scanVars ='", as.character(scanVarsTable[i,5]),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadContinuousVariables <- function(continuousVarsTable, con){
	temp1 <- names(continuousVarsTable)[-(1:3)]
	temp2 <- fixHeader(temp1)
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
			tableHeaders <- temp2[-which(nchar(temp1)==regexpr("[*]", temp1))]
		} else {
			tableHeaders <- temp2
		}

	for (i in 1:nrow(continuousVarsTable)){
		temp3 <- unlist(as.list(continuousVarsTable[i,4:ncol(continuousVarsTable)]))[match(tableHeaders, temp2)]
		command <- paste("INSERT INTO main_tables.continuous_focal_variables(device_ID, focal_start_time, ",
		paste(tableHeaders, collapse=", "),
	")
    SELECT 
    '",as.character(continuousVarsTable[i,]$device_ID),"',
    '", timeFormat(as.character(continuousVarsTable[i,]$focal_start_timeStamp)),"',
    ", paste0("'",paste(as.character(temp3), collapse="', '"),"'"),"
	WHERE NOT EXISTS (SELECT 1 from main_tables.continuous_focal_variables WHERE device_ID='",as.character(continuousVarsTable[i,]$device_ID),"' AND focal_start_time ='",timeFormat(as.character(continuousVarsTable[i,]$focal_start_timeStamp)),"' AND continuousVars ='", as.character(continuousVarsTable[i,4]),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadFocalVariables <- function(focalVarsTable, con){
	temp1 <- names(focalVarsTable)[-(1:3)]
	temp2 <- fixHeader(temp1)
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
			tableHeaders <- temp2[-which(nchar(temp1)==regexpr("[*]", temp1))]
		} else {
			tableHeaders <- temp2
		}

	for (i in 1:nrow(focalVarsTable)){
		temp3 <- unlist(as.list(focalVarsTable[i,4:ncol(focalVarsTable)]))[match(tableHeaders, temp2)]
		command <- paste("INSERT INTO main_tables.focal_variables(device_ID, focal_start_time, ",
		paste(tableHeaders, collapse=", "),
	")
    SELECT 
    '",as.character(focalVarsTable[i,]$device_ID),"',
    '", timeFormat(as.character(focalVarsTable[i,]$focal_start_timeStamp)),"',
    ", paste0("'",paste(as.character(temp3), collapse="', '"),"'"),"
	WHERE NOT EXISTS (SELECT 1 from main_tables.focal_variables WHERE device_ID='",as.character(focalVarsTable[i,]$device_ID),"' AND focal_start_time ='",timeFormat(as.character(focalVarsTable[i,]$focal_start_timeStamp)),"' AND focalVars ='", as.character(focalVarsTable[i,4]),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadSessionVariables <- function(sessionVarsTable, con){
	temp1 <- names(sessionVarsTable)[-(1:2)]
	temp2 <- fixHeader(temp1)
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
			tableHeaders <- temp2[-which(nchar(temp1)==regexpr("[*]", temp1))]
		} else {
			tableHeaders <- temp2
		}
		
	for (i in 1:nrow(sessionVarsTable)){
		temp3 <- unlist(as.list(sessionVarsTable[i,3:ncol(sessionVarsTable)]))[match(tableHeaders, temp2)]
		command <- paste("INSERT INTO main_tables.session_variables(device_ID, session_start_time, ",
		paste(tableHeaders, collapse=", "),
	")
    SELECT 
    '",as.character(sessionVarsTable[i,]$device_ID),"',
    '", timeFormat(as.character(sessionVarsTable[i,]$session_start_timeStamp)),"',
    ", paste0("'",paste(as.character(temp3), collapse="', '"),"'"),"
	WHERE NOT EXISTS (SELECT 1 from main_tables.session_variables WHERE device_ID='",as.character(sessionVarsTable[i,]$device_ID),"' AND session_start_time ='",timeFormat(as.character(sessionVarsTable[i,]$session_start_timeStamp)),"' AND dayVars ='", as.character(sessionVarsTable[i,3]),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadBackgroundTapsTable <- function(backgroundTapsTable, con){
	tableHeaders <- fixHeader(names(backgroundTapsTable))
	for (i in 1:nrow(backgroundTapsTable)){
		command <- paste("INSERT INTO main_tables.list_background_taps(device_ID, focal_start_time, tap_time, description, latitude, longitude, gps_horizontal_precision, altitude)
    SELECT 
    '",as.character(backgroundTapsTable[i,]$device_ID),"',
    '", timeFormat(as.character(backgroundTapsTable[i,]$focal_start_timeStamp)),"',
    '", timeFormat(as.character(backgroundTapsTable[i,]$backgroundTap_timeStamp)),"',
    '", as.character(backgroundTapsTable[i,]$description),"',
	", naFormat(as.character(backgroundTapsTable[i,]$latitude)),",
    ", naFormat(as.character(backgroundTapsTable[i,]$longitude)),",
    ", naFormat(as.character(backgroundTapsTable[i,]$gps_horizontal_precision)),",
    ", naFormat(as.character(backgroundTapsTable[i,]$altitude)),"

	WHERE NOT EXISTS (SELECT 1 from main_tables.list_background_taps WHERE device_ID='",as.character(backgroundTapsTable[i,]$device_ID),"' AND focal_start_time ='",timeFormat(as.character(backgroundTapsTable[i,]$focal_start_timeStamp)),"' AND tap_time ='",timeFormat(as.character(backgroundTapsTable[i,]$backgroundTap_timeStamp)),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}


uploadCommentTable <- function(commentsTable, con){
	tableHeaders <- fixHeader(names(commentsTable))
	for (i in 1:nrow(commentsTable)){
		temp <- gsub("'", "`", as.character(commentsTable[i,]$comment_text))
		command <- paste("INSERT INTO main_tables.list_comments(device_ID, focal_start_time, comment_time, comment)
    SELECT 
    '",as.character(commentsTable[i,]$device_ID),"',
    '", timeFormat(as.character(commentsTable[i,]$focal_start_timeStamp)),"',
    '", timeFormat(as.character(commentsTable[i,]$comment_timeStamp)),"',
    '", temp,"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_comments WHERE device_ID='",as.character(commentsTable[i,]$device_ID),"' AND focal_start_time ='",timeFormat(as.character(commentsTable[i,]$focal_start_timeStamp)),"' AND comment_time ='",timeFormat(as.character(commentsTable[i,]$comment_timeStamp)),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadScansIntermediateTables <- function(scansTable, con){
	temp1 <- names(scansTable)[-c(1:5, (ncol(scansTable)-7):ncol(scansTable))]
	temp2 <- fixHeader(temp1)
	
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
	
	tableHeaders <- temp2[which(nchar(temp1)==regexpr("[*]", temp1))]


	for (i in 1:nrow(scansTable)){
		temp3 <- unlist(as.list(scansTable[i,6:(ncol(scansTable)-8)]))[match(tableHeaders, temp2)]
		for(j in 1:length(tableHeaders)){
		vecValues <- unlist(strsplit(as.character(temp3[j]), ";"))
		if(length(na.omit(vecValues))>0){
			for(k in 1:length(vecValues)){
			command <- paste0("INSERT INTO accessory_tables.scan_data_", tableHeaders[j],"(device_ID, scan_time, scanned_individual_ID, ", tableHeaders[j],")
   			 SELECT 
   			 '",as.character(scansTable[i,]$device_ID),"',
   			 '", timeFormat(as.character(scansTable[i,]$scan_timeStamp)),"',
 			 '", as.character(scansTable[i,]$scanned_individual_ID),"',
			 '", vecValues[k],"'	
			WHERE NOT EXISTS (SELECT 1 from accessory_tables.scan_data_",tableHeaders[j]," WHERE device_ID='",as.character(scansTable[i,]$device_ID),"' AND 	scan_time='",timeFormat(as.character(scansTable[i,]$scan_timeStamp)),"' AND scanned_individual_ID ='",as.character(scansTable[i,]$scanned_individual_ID),"' AND ",tableHeaders[j]," ='",vecValues[k],"');")
			command <- gsub("''", "NULL", command)
			#command <- gsub("NA", "NULL", command)
			command <- gsub("'NULL'", "NULL", command)
			command <- gsub("'NA'", "NULL", command)
			dbGetQuery(con, command)
		}
		}
	}
	}
}
}

uploadBehaviorsIntermediateTables <- function(behaviorsTable, con){
	temp1 <- names(behaviorsTable)[-c(1:6, (ncol(behaviorsTable)-4):ncol(behaviorsTable))]
	temp2 <- fixHeader(temp1)
	
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
	
	tableHeaders <- temp2[which(nchar(temp1)==regexpr("[*]", temp1))]


	for (i in 1:nrow(behaviorsTable)){
		temp3 <- unlist(as.list(behaviorsTable[i,7:(ncol(behaviorsTable)-5)]))[match(tableHeaders, temp2)]
		for(j in 1:length(tableHeaders)){
		vecValues <- unlist(strsplit(as.character(temp3[j]), ";"))
		if(length(na.omit(vecValues))>0){
			for(k in 1:length(vecValues)){
			command <- paste0("INSERT INTO accessory_tables.list_behaviors_", tableHeaders[j],"(device_ID, behavior_time, actor, subject, ", tableHeaders[j],")
   			 SELECT 
   			 '",as.character(behaviorsTable[i,]$device_ID),"',
   			 '", timeFormat(as.character(behaviorsTable[i,]$behavior_timeStamp)),"',
   			 '", as.character(behaviorsTable[i,]$actor),"',
   			 '", as.character(behaviorsTable[i,]$subject),"',
			 '", vecValues[k],"'	
			WHERE NOT EXISTS (SELECT 1 from accessory_tables.list_behaviors_",tableHeaders[j]," WHERE device_ID='",as.character(behaviorsTable[i,]$device_ID),"' AND behavior_time='",timeFormat(as.character(behaviorsTable[i,]$behavior_timeStamp)),"' AND actor='",as.character(behaviorsTable[i,]$actor),"' AND	subject='",as.character(behaviorsTable[i,]$subject),"' AND ",tableHeaders[j]," ='",vecValues[k],"');")
			command <- gsub("''", "NULL", command)
			#command <- gsub("NA", "NULL", command)
			command <- gsub("'NULL'", "NULL", command)
			command <- gsub("'NA'", "NULL", command)
			dbGetQuery(con, command)
		}
		}
	}
	}
}
}

uploadscanVarsIntermediateTables <- function(scanVarsTable, con){
	temp1 <- names(scanVarsTable)[-(1:4)]
	temp2 <- fixHeader(temp1)
	
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
	
	tableHeaders <- temp2[which(nchar(temp1)==regexpr("[*]", temp1))]

	for (i in 1:nrow(scanVarsTable)){
		temp3 <- unlist(as.list(scanVarsTable[i,5:ncol(scanVarsTable)]))[match(tableHeaders, temp2)]
		for(j in 1:length(tableHeaders)){
		vecValues <- unlist(strsplit(as.character(temp3[j]), ";"))
		if(length(na.omit(vecValues))>0){
			for(k in 1:length(vecValues)){
			command <- paste0("INSERT INTO accessory_tables.scan_variables_", tableHeaders[j],"(device_ID, scan_time, scanVars, ", tableHeaders[j],")
   			SELECT 
			'",as.character(scanVarsTable[i,]$device_ID),"',
 			'", timeFormat(as.character(scanVarsTable[i,]$scan_timeStamp)),"',
 			'",as.character(scanVarsTable[i,]$scanVars),"',
			'", vecValues[k],"'	
			WHERE NOT EXISTS (SELECT 1 from accessory_tables.scan_variables_",tableHeaders[j]," WHERE device_ID='",as.character(scanVarsTable[i,]$device_ID),"' AND scan_time='",timeFormat(as.character(scanVarsTable[i,]$scan_timeStamp)),"' AND scanVars='",as.character(scanVarsTable[i,]$scanVars),"' AND ",tableHeaders[j]," ='",vecValues[k],"');")
			command <- gsub("''", "NULL", command)
			#command <- gsub("NA", "NULL", command)
			command <- gsub("'NULL'", "NULL", command)
			command <- gsub("'NA'", "NULL", command)
			dbGetQuery(con, command)
		}
		}
	}
	}
}
}

uploadfocalVarsIntermediateTables <- function(focalVarsTable, con){
	temp1 <- names(focalVarsTable)[-(1:3)]
	temp2 <- fixHeader(temp1)
	
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
	
	tableHeaders <- temp2[which(nchar(temp1)==regexpr("[*]", temp1))]

	for (i in 1:nrow(focalVarsTable)){
		temp3 <- unlist(as.list(focalVarsTable[i,4:ncol(focalVarsTable)]))[match(tableHeaders, temp2)]
		for(j in 1:length(tableHeaders)){
		vecValues <- unlist(strsplit(as.character(temp3[j]), ";"))
		if(length(na.omit(vecValues))>0){
			for(k in 1:length(vecValues)){
			command <- paste0("INSERT INTO accessory_tables.focal_variables_", tableHeaders[j],"(device_ID, focal_start_time, focalVars, ", tableHeaders[j],")
   			SELECT 
			'", as.character(focalVarsTable[i,]$device_ID),"',
   			'", timeFormat(as.character(focalVarsTable[i,]$focal_start_timeStamp)),"',
     		'", as.character(focalVarsTable[i,]$focalVars),"',
			'", vecValues[k],"'	
			WHERE NOT EXISTS (SELECT 1 from accessory_tables.focal_variables_",tableHeaders[j]," WHERE device_ID='",as.character(focalVarsTable[i,]$device_ID),"' AND focal_start_time='",timeFormat(as.character(focalVarsTable[i,]$focal_start_timeStamp)),"' AND focalVars='",as.character(focalVarsTable[i,]$focalVars),"' AND ",tableHeaders[j]," ='",vecValues[k],"');")
			command <- gsub("''", "NULL", command)
			#command <- gsub("NA", "NULL", command)
			command <- gsub("'NULL'", "NULL", command)
			command <- gsub("'NA'", "NULL", command)
			dbGetQuery(con, command)
		}
		}
	}
	}
}
}

uploadContinuousVarsIntermediateTables <- function(continuousVarsTable, con){
	temp1 <- names(continuousVarsTable)[-(1:3)]
	temp2 <- fixHeader(temp1)
	
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
	
	tableHeaders <- temp2[which(nchar(temp1)==regexpr("[*]", temp1))]

	for (i in 1:nrow(continuousVarsTable)){
		temp3 <- unlist(as.list(continuousVarsTable[i,4:ncol(continuousVarsTable)]))[match(tableHeaders, temp2)]
		for(j in 1:length(tableHeaders)){
		vecValues <- unlist(strsplit(as.character(temp3[j]), ";"))
		if(length(na.omit(vecValues))>0){
			for(k in 1:length(vecValues)){
			command <- paste0("INSERT INTO accessory_tables.continuous_focal_variables_", tableHeaders[j],"(device_ID, focal_start_time, continuousVars, ", tableHeaders[j],")
   			SELECT 
			'", as.character(continuousVarsTable[i,]$device_ID),"',
   			'", timeFormat(as.character(continuousVarsTable[i,]$focal_start_timeStamp)),"',
     		'", as.character(continuousVarsTable[i,]$continuousVars),"',
			'", vecValues[k],"'	
			WHERE NOT EXISTS (SELECT 1 from accessory_tables.continuous_focal_variables_",tableHeaders[j]," WHERE device_ID='",as.character(continuousVarsTable[i,]$device_ID),"' AND focal_start_time='",timeFormat(as.character(continuousVarsTable[i,]$focal_start_timeStamp)),"' AND continuousVars ='",as.character(continuousVarsTable[i,]$continuousVars),"' AND ",tableHeaders[j]," ='",vecValues[k],"');")
			command <- gsub("''", "NULL", command)
			#command <- gsub("NA", "NULL", command)
			command <- gsub("'NULL'", "NULL", command)
			command <- gsub("'NA'", "NULL", command)
			dbGetQuery(con, command)
		}
		}
	}
	}
}
}


uploadSessionVarsIntermediateTables <- function(sessionVarsTable, con){
	temp1 <- names(sessionVarsTable)[-(1:2)]
	temp2 <- fixHeader(temp1)
	
	if(length(which(nchar(temp1)==regexpr("[*]", temp1)))>0){
	
	tableHeaders <- temp2[which(nchar(temp1)==regexpr("[*]", temp1))]

	for (i in 1:nrow(sessionVarsTable)){
		temp3 <- unlist(as.list(sessionVarsTable[i,3:ncol(sessionVarsTable)]))[match(tableHeaders, temp2)]
		for(j in 1:length(tableHeaders)){
		vecValues <- unlist(strsplit(as.character(temp3[j]), ";"))
		if(length(na.omit(vecValues))>0){
			for(k in 1:length(vecValues)){
			command <- paste0("INSERT INTO accessory_tables.session_variables_", tableHeaders[j],"(device_ID, session_start_time, dayVars, ", tableHeaders[j],")
   			SELECT 
			'",as.character(sessionVarsTable[i,]$device_ID),"',
   			'", timeFormat(as.character(sessionVarsTable[i,]$session_start_timeStamp)),"',
     		'", as.character(sessionVarsTable[i,]$dayVars),"',
			'", vecValues[k],"'	
			WHERE NOT EXISTS (SELECT 1 from accessory_tables.session_variables_",tableHeaders[j]," WHERE device_ID='",as.character(sessionVarsTable[i,]$device_ID),"' AND session_start_time='",timeFormat(as.character(sessionVarsTable[i,]$session_start_timeStamp)),"' AND dayVars ='",as.character(sessionVarsTable[i,]$dayVars),"' AND ",tableHeaders[j]," ='",vecValues[k],"');")
			command <- gsub("''", "NULL", command)
			#command <- gsub("NA", "NULL", command)
			command <- gsub("'NULL'", "NULL", command)
			command <- gsub("'NA'", "NULL", command)
			dbGetQuery(con, command)
		}
		}
	}
	}
}
}

