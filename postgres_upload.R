timeFormat <- function(stamp){
	paste(unlist(strsplit(stamp, ",")), collapse=" ")
}

uploadDatFile <- function(dataOutput, con){
	sessionsTable <- dataOutput$sessionsTable
	for (i in 1:nrow(sessionsTable)){
		dbGetQuery(con, paste("INSERT INTO main_tables.list_sessions(device_ID, session_start_time, session_end_time, group_ID, pin_code_name, layout_info_json_version, behaviors_json_version, gps_on, compass_on, map_mode_on)
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
    '", as.character(sessionsTable[i,]$map_mode_on),"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_sessions WHERE device_ID='",as.character(sessionsTable[i,]$device_ID),"' AND session_start_time='",timeFormat(as.character(sessionsTable[i,]$session_start_timeStamp)),"');", sep="")
	)
	}
}

