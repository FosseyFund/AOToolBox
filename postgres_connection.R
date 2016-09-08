getTableList <- function(con){
	temp <- dbGetQuery(con, "SELECT schemaname,relname,n_live_tup FROM pg_stat_user_tables ORDER BY schemaname, n_live_tup DESC;")##number of rows, approximate
names(temp) <- c("schema name", "table name", "approximate number of rows")
temp[,3] <- as.integer(temp[,3])
return(temp)
}



#listTables <- jsonOutputConversion(NULL, "~/Documents/git repositories/temp/behaviors.json", "~/Documents/git repositories/temp/layout_info.json")

createListSQLTables <- function(listTables){
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
	sqlCode <- list()
	sqlCode <- c(sqlCode, "create schema animal_observer;")
	sqlCode <- c(sqlCode, paste("create schema animal_observer;"))

	
}



