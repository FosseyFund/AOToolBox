getTableList <- function(con){
	temp <- dbGetQuery(con, "SELECT schemaname,relname,n_live_tup FROM pg_stat_user_tables ORDER BY schemaname, n_live_tup DESC;")##number of rows, approximate
names(temp) <- c("schema name", "table name", "approximate number of rows")
temp[,3] <- as.integer(temp[,3])
return(temp)
}



#listTables <- jsonOutputConversion(NULL, "~/Documents/git repositories/temp/behaviors.json", "~/Documents/git repositories/temp/layout_info.json")

# createListSQLTables <- function(listTables, con){
	# #list of headers
	# tableHeaders <- list()
	# tableHeaders[[1]] <- names(listTables$sessionsTable)
	# tableHeaders[[2]] <- names(listTables$focalsTable)
	# tableHeaders[[3]] <- names(listTables$behaviorsTable)
	# tableHeaders[[4]] <- names(listTables$scansTable)
	# tableHeaders[[5]] <- names(listTables$backgroundTapsTable)
	# tableHeaders[[6]] <- names(listTables$commentsTable)
	# tableHeaders[[7]] <- names(listTables$dayVarsTable)
	# tableHeaders[[8]] <- names(listTables$focalVarsTable)
	# tableHeaders[[9]] <- names(listTables$continuousVarsTable)
	# tableHeaders[[10]] <- names(listTables$scanVarsTable)
	
	# dbGetQuery(con, "create database animal_observer;")
	# cat(file=stderr(), "Database animal_observer created...")
	# dbDisconnect(con)
	# #dbConnect(drv=dbDriver("PostgreSQL"), dbname = "animal_observer",
    # #              host = DBhost(), port = DBport(),
    # #             user = DBuser(), password = DBpwd())
	
	# dbDisconnect(con)
	# con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname =  "postgres", host = "localhost", port = 5432, user = "postgres", password = "postgres")
	# dbGetQuery(con, "select pg_terminate_backend(pid) from pg_stat_activity where datname='animal_observer';")##disconnect all users of animal_observer
	# dbGetQuery(con, "drop database if exists animal_observer;")	
	# dbGetQuery(con, "create database animal_observer;")
	# con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname =  "animal_observer", host = "localhost", port = 5432, user = "postgres", password = "postgres")
	# sqlCode <- list()
	# sqlCode <- c(sqlCode, "create schema main_tables;")
	# sqlCode <- c(sqlCode, "drop schema public;")
	# sqlCode <- c(sqlCode, "create schema accessory_tables;")
	# sqlCode <- c(sqlCode, "create table main_tables.list_sessions (
	# device_ID text NOT NULL,
	# session_start_time timestamp NOT NULL,
	# session_end_time timestamp,
	# group_ID text NOT NULL,
	# pin_code_name text NOT NULL,
	# layout_info_json_version	 text NOT NULL,
	# behaviors_json_version text NOT NULL,
	# gps_on boolean NOT NULL,
	# compass_on	boolean NOT NULL,
	# map_mode_on boolean NOT NULL,
	# PRIMARY KEY (device_ID, session_start_time)
	# );")
	# sqlCode <- c(sqlCode, "create table main_tables.list_focals (
	# device_ID text NOT NULL,
	# session_start_time timestamp NOT NULL,
	# focal_start_time  timestamp NOT NULL,
	# focal_end_time timestamp,
	# set_duration interval NOT NULL,
	# set_scan_interval interval NOT NULL,
	# focal_individual_ID text NOT NULL,
	# PRIMARY KEY (device_ID, session_start_time, focal_start_time)
	# );")
	# sqlCode <- c(sqlCode, paste0("create table main_tables.list_behaviors (
	# device_ID text NOT NULL,
	# session_start_time timestamp NOT NULL,
	# focal_start_time  timestamp NOT NULL,
	# behavior_time timestamp NOT NULL,
	# actor text NOT NULL,
	# subject text NOT NULL,",
	# paste(gsub("[.]", "_", tableHeaders[[3]][7:(length(tableHeaders[[3]])-5)]), collapse=" text,\n"),
	# " text,
	# comment text,
	# latitude double precision,
	# longitude double precision,
	# gps_horizontal_precision real,
	# altitude real,
	# PRIMARY KEY (device_ID, behavior_time, actor, subject)
	# );"))
	
	
	# device_ID	session_start_timeStamp	focal_start_timeStamp	timeStamp	scanned_individual_ID	ACTIVITY	HEIGHT	FOOD_ITEM	PART_EATEN	GROOMEE_ID	x_position	y_position	physical_contact_threshold	latitude	longitude	gps_horizontal_precision	altitude	compass_bearing

	# sqlCode <- c(sqlCode, paste0("create table main_tables.list_scans (
	# device_ID text NOT NULL,
	# session_start_time timestamp NOT NULL,
	# focal_start_time  timestamp NOT NULL,
	# scan_time timestamp NOT NULL,
	# scanned_individual_ID text NOT NULL,
	# paste(gsub("[.]", "_", tableHeaders[[4]][6:(length(tableHeaders[[4]])-8)]), collapse=" text,\n"),
	# " text,
	# x_position real NOT NULL,
	# y_position real NOT NULL,
	# latitude double precision,
	# longitude double precision,
	# gps_horizontal_precision real,
	# altitude real,
	# PRIMARY KEY (behavior_time, actor, subject)
	# );"))


	# dbGetQuery(con, paste(unlist(sqlCode), collapse=" "))
	

	# #write(unlist(sqlCode), file="~/Documents/git repositories/temp/test.txt")
	
# }
