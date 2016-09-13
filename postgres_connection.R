getTableList <- function(con){
	temp <- dbGetQuery(con, "SELECT schemaname,relname,n_live_tup FROM pg_stat_user_tables ORDER BY schemaname, n_live_tup DESC;")##number of rows, approximate
names(temp) <- c("schema name", "table name", "approximate number of rows")
temp[,3] <- as.integer(temp[,3])
return(temp)
}


# behaviors.json="~/Documents/git repositories/temp/behaviors.json"

# listTables <- jsonOutputConversion("~/Documents/git repositories/AOToolBox/default json files/Winnie_2016-09-09_16h39m46s.dat.json", "~/Documents/git repositories/AOToolBox/default json files/behaviors.json", "~/Documents/git repositories/AOToolBox/default json files/layout_info.json")


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
	# fixHeader <- function(v)
	# {
		# unlist(lapply(strsplit(make.names(tolower(v), unique=T), "[.]"), function(x) paste(x, collapse="_")))
	# }
	
	# for(i in 1:length(tableHeaders)){
		# tableHeaders[[i]] <- fixHeader(tableHeaders[[i]])
	# }
	
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
	# dbDisconnect(con)
	# con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname =  "animal_observer", host = "localhost", port = 5432, user = "postgres", password = "postgres")
	# sqlCode <- list()
	# sqlCode <- c(sqlCode, "create schema main_tables;")
	# sqlCode <- c(sqlCode, "drop schema public;")
	# sqlCode <- c(sqlCode, "create schema accessory_tables;")
	
	# sqlCode <- c(sqlCode, "CREATE OR REPLACE FUNCTION main_tables.row_modif_stamp() RETURNS trigger AS $BODY$
   	# BEGIN
       # NEW.last_modif_on := current_timestamp;
       # NEW.last_modif_by := current_user;
       # RETURN NEW;
   	# END;
	# $BODY$ LANGUAGE plpgsql;")



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
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
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
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# PRIMARY KEY (device_ID, focal_start_time),
	# FOREIGN KEY (device_ID, session_start_time) REFERENCES main_tables.list_sessions(device_ID, session_start_time) ON UPDATE CASCADE
	# );")
	# sqlCode <- c(sqlCode, paste0("create table main_tables.list_behaviors (
	# device_ID text NOT NULL,
	# focal_start_time  timestamp NOT NULL,
	# behavior_time timestamp NOT NULL,
	# actor text NOT NULL,
	# subject text NOT NULL, ",
	# paste(gsub("[.]", "_", tableHeaders[[3]][7:(length(tableHeaders[[3]])-5)]), collapse=" text,\n"),
	# " text,
	# comment text,
	# latitude double precision,
	# longitude double precision,
	# gps_horizontal_precision real,
	# altitude real,	
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# PRIMARY KEY (device_ID, behavior_time, ",tableHeaders[[3]][7]," , actor, subject),
	# FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	# );"))
	# sqlCode <- c(sqlCode, paste0("create table main_tables.list_scans (
	# device_ID text NOT NULL,
	# focal_start_time  timestamp NOT NULL,
	# scan_time timestamp NOT NULL,
	# latitude double precision,
	# longitude double precision,
	# gps_horizontal_precision real,
	# altitude real,	
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# PRIMARY KEY (device_ID, scan_time),
	# FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	# );"))
	# sqlCode <- c(sqlCode, paste0("create table main_tables.scan_data (
	# device_ID text NOT NULL,
	# scan_time timestamp NOT NULL,
	# scanned_individual_ID text NOT NULL, ",
	# paste(gsub("[.]", "_", tableHeaders[[4]][6:(length(tableHeaders[[4]])-8)]), collapse=" text,\n"),
	# " text,
	# x_position real NOT NULL,
	# y_position real NOT NULL,	
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# PRIMARY KEY (device_ID, scan_time, scanned_individual_ID),
	# FOREIGN KEY (device_ID, scan_time) REFERENCES main_tables.list_scans(device_ID, scan_time) ON UPDATE CASCADE
	# );"))
	# sqlCode <- c(sqlCode, paste0("create table main_tables.scan_variables (
	# device_ID text NOT NULL,
	# scan_time timestamp NOT NULL, ",
	# paste(gsub("[.]", "_", tableHeaders[[10]][5:(length(tableHeaders[[10]]))]), collapse=" text,\n"),
	# " text,	
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# PRIMARY KEY (device_ID, scan_time, scanVars),
	# FOREIGN KEY (device_ID, scan_time) REFERENCES main_tables.list_scans(device_ID, scan_time) ON UPDATE CASCADE
	# );"))
	# sqlCode <- c(sqlCode, paste0("create table main_tables.continuous_focal_variables (
	# device_ID text NOT NULL,
	# focal_start_time  timestamp NOT NULL, ",
	# paste(gsub("[.]", "_", tableHeaders[[9]][4:(length(tableHeaders[[9]]))]), collapse=" text,\n"),
	# " text,	
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# PRIMARY KEY (device_ID, focal_start_time, continuousVars),
	# FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	# );"))
	# sqlCode <- c(sqlCode, paste0("create table main_tables.focal_variables (
	# device_ID text NOT NULL,
	# focal_start_time  timestamp NOT NULL, ",
	# paste(gsub("[.]", "_", tableHeaders[[8]][4:(length(tableHeaders[[8]]))]), collapse=" text,\n"),
	# " text,	
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# PRIMARY KEY (device_ID, focal_start_time, focalVars),
	# FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	# );"))
	# sqlCode <- c(sqlCode, paste0("create table main_tables.session_variables (
	# device_ID text NOT NULL,
	# session_start_time  timestamp NOT NULL, ",
	# paste(gsub("[.]", "_", tableHeaders[[7]][3:(length(tableHeaders[[7]]))]), collapse=" text,\n"),
	# " text,	
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# PRIMARY KEY (device_ID, session_start_time, dayVars),
	# FOREIGN KEY (device_ID, session_start_time) REFERENCES main_tables.list_sessions(device_ID, session_start_time) ON UPDATE CASCADE
	# );"))
	# sqlCode <- c(sqlCode, paste0("create table main_tables.list_background_taps (
	# device_ID text NOT NULL,
	# focal_start_time  timestamp NOT NULL,
	# tap_time timestamp NOT NULL,
	# description text,
	# latitude double precision,
	# longitude double precision,
	# gps_horizontal_precision real,
	# altitude real,	
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# PRIMARY KEY (device_ID, focal_start_time, tap_time),
	# FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	# );"))
	# sqlCode <- c(sqlCode, paste0("create table main_tables.list_comments (
	# device_ID text NOT NULL,
	# focal_start_time  timestamp NOT NULL,
	# comment_time timestamp NOT NULL,
	# comment text,
	# latitude double precision,
	# longitude double precision,
	# gps_horizontal_precision real,
	# altitude real,	
	# created_by text DEFAULT CURRENT_USER,
	# created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# last_modif_by text DEFAULT CURRENT_USER,
	# last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	# PRIMARY KEY (device_ID, focal_start_time, comment_time),
	# FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	# );"))
	# for(i in c(
	# "main_tables.list_sessions",
	# "main_tables.list_focals",
	# "main_tables.list_behaviors",
	# "main_tables.list_scans",
	# "main_tables.scan_data",
	# "main_tables.scan_variables",
	# "main_tables.continuous_focal_variables",
	# "main_tables.focal_variables",
	# "main_tables.session_variables",
	# "main_tables.list_background_taps",
	# "main_tables.list_comments")){
		# sqlCode <- c(sqlCode, paste("CREATE TRIGGER row_modif_stamp AFTER INSERT OR UPDATE ON",i, "FOR EACH ROW EXECUTE PROCEDURE main_tables.row_modif_stamp();"))
	# }

	
	# dbGetQuery(con, paste(unlist(sqlCode), collapse=" "))
	
	

	# #write(unlist(sqlCode), file="~/Documents/git repositories/temp/test.txt")
	
# }
