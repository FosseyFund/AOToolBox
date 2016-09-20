timeFormat <- function(stamp){
	paste(unlist(strsplit(stamp, ",")), collapse=" ")
}

uploadSessionsTable <- function(sessionsTable, con){
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

"create table main_tables.list_focals (
	device_ID text NOT NULL,
	session_start_time timestamp NOT NULL,
	focal_start_time  timestamp NOT NULL,
	focal_end_time timestamp,
	set_duration interval NOT NULL,
	set_scan_interval interval NOT NULL,
	focal_individual_ID text NOT NULL,	
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, focal_start_time),
	FOREIGN KEY (device_ID, session_start_time) REFERENCES main_tables.list_sessions(device_ID, session_start_time) ON UPDATE CASCADE
	);"

uploadFocalsTable <- function(focalsTable, con){
	for (i in 1:nrow(focalsTable)){
		dbGetQuery(con, paste("INSERT INTO main_tables.list_focals(device_ID, session_start_time, focal_start_time, focal_end_time, set_duration, set_scan_interval, focal_individual_ID)
    SELECT 
    '",as.character(focalsTable[i,]$device_ID),"',
    '",timeFormat(as.character(focalsTable[i,]$session_start_timeStamp)),"',
    '",timeFormat(as.character(focalsTable[i,]$focal_start_timeStamp)),"',
    '", as.character(focalsTable[i,]$focal_end_timeStamp),"',
    '", as.character(focalsTable[i,]$focal_set_duration),"',
    '", as.character(focalsTable[i,]$focal_set_scan_interval),"',
    '", as.character(focalsTable[i,]$focal_individual_ID),"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_focals WHERE device_ID='",as.character(focalsTable[i,]$device_ID),"' AND focal_start_time='",timeFormat(as.character(focalsTable[i,]$focal_start_timeStamp)),"');", sep="")
	)
	}
}

uploadBehaviorsTable <- function(behaviorsTable, con){
	tableHeaders <- fixHeader(names(behaviorsTable))
	for (i in 1:nrow(behaviorsTable)){
		command <- paste("INSERT INTO main_tables.list_behaviors(device_ID, focal_start_time, behavior_time, actor, subject, ",
	paste(gsub("[.]", "_", tableHeaders[7:(length(tableHeaders)-5)]), collapse=", "),
	", comment, latitude, longitude, gps_horizontal_precision, altitude)
    SELECT 
    '",as.character(behaviorsTable[i,]$device_ID),"',
    '",timeFormat(as.character(behaviorsTable[i,]$focal_start_timeStamp)),"',
    '", timeFormat(as.character(behaviorsTable[i,]$behavior_timeStamp)),"',
    '", as.character(behaviorsTable[i,]$actor),"',
    '", as.character(behaviorsTable[i,]$subject),"',
    '", paste(as.character(unlist(as.list(behaviorsTable[i,7:(length(tableHeaders)-5)]))), collapse="', '"),"',
    '", as.character(behaviorsTable[i,]$comment),"',
    ", as.character(behaviorsTable[i,]$latitude),",
    ", as.character(behaviorsTable[i,]$longitude),",
    ", as.character(behaviorsTable[i,]$gps_horizontal_precision),",
    ", as.character(behaviorsTable[i,]$altitude),"
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_behaviors WHERE device_ID='",as.character(behaviorsTable[i,]$device_ID),"' AND behavior_time='",timeFormat(as.character(behaviorsTable[i,]$behavior_timeStamp)),"' AND actor='",as.character(behaviorsTable[i,]$actor),"' AND	subject='",as.character(behaviorsTable[i,]$subject),"');", sep="")
		#command <- gsub(command, "''", "NULL")
		dbGetQuery(con, command)
	}
}





