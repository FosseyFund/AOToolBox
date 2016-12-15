getTableList <- function(con, database){
	temp <- dbGetQuery(con, "SELECT schemaname,relname,n_live_tup FROM pg_stat_user_tables ORDER BY schemaname, n_live_tup DESC;")##number of rows, approximate
if(nrow(temp)>0){
names(temp) <- c("schema name", "table name", "approximate number of rows")
temp[,3] <- as.integer(temp[,3])
} else {
	temp <- data.frame(Report=paste0("Database '",database,"' is empty"))
}
return(temp)
}

fixHeader <- function(v)
	{
		unlist(lapply(strsplit(make.names(tolower(v), unique=T), "[.]"), function(x) paste(x, collapse="_")))
	}
	
sqlCodeSmallTable <- function(lsvars, largeTable="main_tables.list_behaviors"){
	ans <- list()
	for(i in 1:length(lsvars)){
	#tableName <- gsub("[.]","_",gsub("[.][.]",".",make.names(names(lsvars[i]))))
	tableName <- fixHeader(names(lsvars[i]))
	ans <- c(ans, paste0("create table IF NOT EXISTS accessory_tables.", tableName," (
	value text PRIMARY KEY,
	description text,
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP
	);",
	"DROP TRIGGER IF EXISTS row_modif_stamp ON accessory_tables.", tableName,";",
	"CREATE TRIGGER row_modif_stamp BEFORE INSERT OR UPDATE ON accessory_tables.", tableName," FOR EACH ROW EXECUTE PROCEDURE main_tables.row_modif_stamp();
	ALTER TABLE ", largeTable," ADD FOREIGN KEY (", fixHeader(names(lsvars[i])),") REFERENCES accessory_tables.", tableName,"(value) ON UPDATE CASCADE;")
	)
	for (j in lsvars[[i]]){
	command <- paste0("INSERT INTO accessory_tables.", tableName,"(value)
	SELECT
	'",j,"' WHERE NOT EXISTS (SELECT 1 from accessory_tables.",tableName," WHERE value='",j,"');")
	ans <- c(ans, command)
	}
}
	return(ans)
}

createListSQLTables <- function(behav, layout, colmerge, con, newdbname, username, hostname, pwd){
	listTables <- jsonOutputConversion(json.output.file =NULL, behav, layout, colmerge=colmerge)
	#list of headers
	tableHeaders <- list()
	tableHeaders[[1]] <- names(listTables$sessionsTable)
	tableHeaders[[2]] <- names(listTables$focalsTable)
	tableHeaders[[3]] <- names(listTables$behaviorsTable)
	tableHeaders[[4]] <- names(listTables$scansTable)
	tableHeaders[[5]] <- names(listTables$backgroundTapsTable)
	tableHeaders[[6]] <- names(listTables$commentsTable)
	tableHeaders[[7]] <- names(listTables$dayVarsTable)
	tableHeaders[[8]] <- names(listTables$focalVarsTable)
	tableHeaders[[9]] <- names(listTables$continuousVarsTable)
	tableHeaders[[10]] <- names(listTables$scanVarsTable)
	
	multipleSelectionCols <- tableHeaders
	multipleSelectionCols2 <- lapply(multipleSelectionCols, function(temp) which(nchar(temp)==regexpr("[*]", temp)))
	tableHeadersAll <- tableHeaders
	#all_cons <- dbListConnections(dbDriver("PostgreSQL"))
    #for(con in all_cons) dbDisconnect(con)
	
	for(i in 1:length(tableHeaders)){
		if(length(multipleSelectionCols2[[i]])==0) tableHeaders[[i]] <- fixHeader(tableHeaders[[i]]) else
		tableHeaders[[i]] <- fixHeader(tableHeaders[[i]][-multipleSelectionCols2[[i]]])
	}
	
	if(newdbname %in% dbGetQuery(con, "SELECT datname FROM pg_database WHERE datistemplate = false;")[,1]) {
		cat(file=stderr(), paste0("Database ", newdbname," already exists!\n"))
		return(NULL)
		}
		
	

	#con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname =  "postgres", host = "localhost", port = 5432, user = "postgres", password = "postgres")
	#dbGetQuery(con, paste0("select pg_terminate_backend(pid) from pg_stat_activity where datname=", newdbname,"'animal_observer';"))##disconnect all users of animal_observer
	#dbGetQuery(con, "drop database if exists animal_observer;")	
	dbGetQuery(con, paste0("create database ",newdbname,";"))
	all_cons <- dbListConnections(dbDriver("PostgreSQL"))
    for(con in all_cons) dbDisconnect(con)
	con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname =  newdbname, host = hostname, port = 5432, user = username, password = pwd)
	sqlCode <- list()
	sqlCode <- c(sqlCode, "create schema main_tables; create schema accessory_tables; SET client_min_messages = error;")
	sqlCode <- c(sqlCode, "drop schema public;")
	#sqlCode <- c(sqlCode, "create schema accessory_tables;")
	
	sqlCode <- c(sqlCode, "CREATE OR REPLACE FUNCTION main_tables.row_modif_stamp() RETURNS trigger AS $BODY$
   	BEGIN
       NEW.last_modif_on := current_timestamp;
       NEW.last_modif_by := current_user;
       RETURN NEW;
   	END;
	$BODY$ LANGUAGE plpgsql;")

	sqlCode <- c(sqlCode, "create table main_tables.list_sessions (
	device_ID text NOT NULL,
	session_start_time timestamp NOT NULL,
	session_end_time timestamp,
	group_ID text NOT NULL,
	pin_code_name text NOT NULL,
	layout_info_json_version text NOT NULL,
	behaviors_json_version text NOT NULL,
	gps_on boolean NOT NULL,
	compass_on	boolean NOT NULL,
	map_mode_on boolean NOT NULL,
	physical_contact_threshold real NOT NULL,
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, session_start_time)
	);")
	sqlCode <- c(sqlCode, "create table main_tables.list_focals (
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
	);")
	sqlCode <- c(sqlCode, paste0("create table main_tables.list_behaviors (
	device_ID text NOT NULL,
	focal_start_time  timestamp NOT NULL,
	behavior_time timestamp NOT NULL,
	actor text NOT NULL,
	subject text NOT NULL, ",
	paste(gsub("[.]", "_", tableHeaders[[3]][7:(length(tableHeaders[[3]])-5)]), collapse=" text,\n"),
	" text,
	comment text,
	latitude double precision,
	longitude double precision,
	gps_horizontal_precision real,
	altitude real,	
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, behavior_time, actor, subject),
	FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	);"))
	sqlCode <- c(sqlCode, paste0("create table main_tables.list_scans (
	device_ID text NOT NULL,
	focal_start_time  timestamp NOT NULL,
	scan_time timestamp NOT NULL,
	latitude double precision,
	longitude double precision,
	gps_horizontal_precision real,
	altitude real,	
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, scan_time),
	FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	);"))
	sqlCode <- c(sqlCode, paste0("create table main_tables.scan_data (
	device_ID text NOT NULL,
	scan_time timestamp NOT NULL,
	scanned_individual_ID text NOT NULL, ",
	paste(gsub("[.]", "_", tableHeaders[[4]][6:(length(tableHeaders[[4]])-8)]), collapse=" text,\n"),
	" text,
	x_position real NOT NULL,
	y_position real NOT NULL,	
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, scan_time, scanned_individual_ID),
	FOREIGN KEY (device_ID, scan_time) REFERENCES main_tables.list_scans(device_ID, scan_time) ON UPDATE CASCADE
	);"))
	sqlCode <- c(sqlCode, paste0("create table main_tables.scan_variables (
	device_ID text NOT NULL,
	scan_time timestamp NOT NULL, ",
	paste(gsub("[.]", "_", tableHeaders[[10]][5:(length(tableHeaders[[10]]))]), collapse=" text,\n"),
	" text,	
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, scan_time, scanVars),
	FOREIGN KEY (device_ID, scan_time) REFERENCES main_tables.list_scans(device_ID, scan_time) ON UPDATE CASCADE
	);"))
	if(length(tableHeaders[[9]])>3){
	sqlCode <- c(sqlCode, paste0("create table main_tables.continuous_focal_variables (
	device_ID text NOT NULL,
	focal_start_time  timestamp NOT NULL, ",
	paste0(paste(gsub("[.]", "_", tableHeaders[[9]][4:(length(tableHeaders[[9]]))]), collapse=" text,\n"), " text,\n"),
	"created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, focal_start_time, continuousVars),
	FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	);"))
	}
	sqlCode <- c(sqlCode, paste0("create table main_tables.focal_variables (
	device_ID text NOT NULL,
	focal_start_time  timestamp NOT NULL, ",
	paste(gsub("[.]", "_", tableHeaders[[8]][4:(length(tableHeaders[[8]]))]), collapse=" text,\n"),
	" text,	
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, focal_start_time, focalVars),
	FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	);"))
	sqlCode <- c(sqlCode, paste0("create table main_tables.session_variables (
	device_ID text NOT NULL,
	session_start_time  timestamp NOT NULL, ",
	paste(gsub("[.]", "_", tableHeaders[[7]][3:(length(tableHeaders[[7]]))]), collapse=" text,\n"),
	" text,	
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, session_start_time, dayVars),
	FOREIGN KEY (device_ID, session_start_time) REFERENCES main_tables.list_sessions(device_ID, session_start_time) ON UPDATE CASCADE
	);"))
	sqlCode <- c(sqlCode, paste0("create table main_tables.list_background_taps (
	device_ID text NOT NULL,
	focal_start_time  timestamp NOT NULL,
	tap_time timestamp NOT NULL,
	description text,
	latitude double precision,
	longitude double precision,
	gps_horizontal_precision real,
	altitude real,	
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, focal_start_time, tap_time),
	FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	);"))
	sqlCode <- c(sqlCode, paste0("create table main_tables.list_comments (
	device_ID text NOT NULL,
	focal_start_time  timestamp NOT NULL,
	comment_time timestamp NOT NULL,
	comment text,
	latitude double precision,
	longitude double precision,
	gps_horizontal_precision real,
	altitude real,	
	created_by text DEFAULT CURRENT_USER,
	created_on timestamp DEFAULT CURRENT_TIMESTAMP,
	last_modif_by text DEFAULT CURRENT_USER,
	last_modif_on timestamp DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (device_ID, focal_start_time, comment_time),
	FOREIGN KEY (device_ID, focal_start_time) REFERENCES main_tables.list_focals(device_ID, focal_start_time) ON UPDATE CASCADE
	);"))
	
		
	#################primary key tables
	smallTables <- small_tables(behav, layout)
	###case of solo when headers ar merged
	if(colmerge)
	{
		overlapping <- intersect(names(smallTables$dyadic),names(smallTables$solo))
		clade <- c(smallTables$dyadic[!names(smallTables$dyadic)%in%overlapping],smallTables$solo[!names(smallTables$solo)%in%overlapping]) 
		for (i in overlapping){
			clade <- c(clade, list(union(smallTables$dyadic[names(smallTables$dyadic)==i][[1]], smallTables$solo[names(smallTables$solo)==i][[1]])))
			names(clade)[length(clade)] <- i
		}
		finalNames <- names(c(smallTables$dyadic, smallTables$solo))
		finalNames <- finalNames[!duplicated(finalNames)]
		clade <- clade[match(finalNames,names(clade))]
	} else {
		clade <- c(smallTables$dyadic, smallTables$solo)
		#names(clade) <- fixHeader(names(clade))
	}

	isStar <- function(v){
			nchar(names(v))==regexpr("[*]", names(v))
	}
	sqlCode <- c(sqlCode, sqlCodeSmallTable(smallTables$scan[!isStar(smallTables$scan)], largeTable="main_tables.scan_data"))
	sqlCode <- c(sqlCode, sqlCodeSmallTable(smallTables$sessionVars[!isStar(smallTables$sessionVars)], largeTable="main_tables.session_variables"))
	sqlCode <- c(sqlCode, sqlCodeSmallTable(smallTables$focalVars[!isStar(smallTables$focalVars)], largeTable="main_tables.focal_variables"))
	sqlCode <- c(sqlCode, sqlCodeSmallTable(smallTables$scanVars[!isStar(smallTables$scanVars)], largeTable="main_tables.scan_variables"))	
	if(!is.null(smallTables$continuousVars)) sqlCode <- c(sqlCode, sqlCodeSmallTable(smallTables$continuousVars[!isStar(smallTables$continuousVars)], largeTable="main_tables.continuous_focal_variables"))
	
	sqlCode <- c(sqlCode, sqlCodeSmallTable(clade[!isStar(clade)], largeTable="main_tables.list_behaviors"))
	
#######normalization of multiple selection columns
	tabList <- data.frame(index=c(3,4,7:10), tabName=c("list_behaviors", "list_scans", "session_variables", "focal_variables", "continuous_focal_variables","scan_variables"),
	pK=c("device_ID, behavior_time, actor, subject",
	"device_ID, scan_time, scanned_individual_ID",
	"device_ID, session_start_time, dayVars",
	"device_ID, focal_start_time, focalVars",
	"device_ID, focal_start_time, continuousVars",
	"device_ID, scan_time, scanVars"), pkVars=c("device_ID text,
	behavior_time timestamp,
	actor text,
	subject text",
	"device_ID text,
	scan_time timestamp,
	scanned_individual_ID text",
	"device_ID text,
	session_start_time timestamp,
	dayVars text",
	"device_ID text,
	focal_start_time  timestamp, 
	focalVars text",
	"device_ID text,
	focal_start_time  timestamp,
	continuousVars text",
	"device_ID text,
	scan_time timestamp, 
	scanVars text"))
	
	
	for(i in tabList[-1,1]){
		if(length(multipleSelectionCols2[[i]])!=0){
			for (j in multipleSelectionCols2[[i]]){
			varName <- fixHeader(tableHeadersAll[[i]][j])
			sqlCode <- c(sqlCode, paste0("create table IF NOT EXISTS accessory_tables.",tabList[tabList[,1]==i,2],"_", varName, " (
			", tabList[tabList[,1]==i,4], ",
			", varName," text,
			PRIMARY KEY (",paste(tabList[tabList[,1]==i,3],",",varName),"),
			FOREIGN KEY (",tabList[tabList[,1]==i,3],") REFERENCES main_tables.",tabList[tabList[,1]==i,2],"(",tabList[tabList[,1]==i,3],") ON UPDATE CASCADE,
			FOREIGN KEY (",varName,") REFERENCES accessory_tables.",varName,"(value) ON UPDATE CASCADE
			)"
			))
			}
		}
	}
	##case of list_behaviors
	if(colmerge){
		
	multipleSelected <- which(nchar(names(clade))==regexpr("[*]",names(clade)))
	if(length(multipleSelected)!=0){
			for (j in multipleSelected){
			varName <- fixHeader(names(clade)[j])
			sqlCode <- c(sqlCode, paste0("create table IF NOT EXISTS accessory_tables.",tabList[tabList[,1]==i,2],"_", varName, " (
			", tabList[tabList[,1]==i,4], ",
			", varName," text,
			PRIMARY KEY (",paste(tabList[tabList[,1]==i,3],",",varName),"),
			FOREIGN KEY (",tabList[tabList[,1]==i,3],") REFERENCES main_tables.",tabList[tabList[,1]==i,2],"(",tabList[tabList[,1]==i,3],") ON UPDATE CASCADE,
			FOREIGN KEY (",varName,") REFERENCES accessory_tables.",varName,"(value) ON UPDATE CASCADE
			)"
			))
			}
		}
	}
	
	multipleSelectionCols <- tableHeadersAll
	multipleSelectionCols2 <- lapply(multipleSelectionCols, function(temp) which(nchar(temp)==regexpr("[*]", temp)))
	
	
	
	##################
	for(i in c(na.omit(c(
	"main_tables.list_sessions",
	"main_tables.list_focals",
	"main_tables.list_behaviors",
	"main_tables.list_scans",
	"main_tables.scan_data",
	"main_tables.scan_variables",
	ifelse(length(tableHeaders[[9]])>3,"main_tables.continuous_focal_variables",NA),
	"main_tables.focal_variables",
	"main_tables.session_variables",
	"main_tables.list_background_taps",
	"main_tables.list_comments")))){
		sqlCode <- c(sqlCode, paste("CREATE TRIGGER row_modif_stamp BEFORE INSERT OR UPDATE ON",i, "FOR EACH ROW EXECUTE PROCEDURE main_tables.row_modif_stamp();"))
	}

	dbGetQuery(con, paste(unlist(sqlCode), collapse=" "))
	
	cat(file=stderr(), paste0("Database ", newdbname ," created!\n"))
	return("success")
	#write(unlist(sqlCode), file="~/Downloads/test.txt")
	
}







