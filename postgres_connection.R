getTableList <- function(con){
	temp <- dbGetQuery(con, "SELECT schemaname,relname,n_live_tup FROM pg_stat_user_tables ORDER BY schemaname, n_live_tup DESC;")##number of rows, approximate
names(temp) <- c("schema name", "table name", "number of rows")
temp[,3] <- as.integer(temp[,3])
return(temp)
	
}