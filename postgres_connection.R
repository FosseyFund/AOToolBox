getTableList <- function(con){
	temp <- dbGetQuery(con, "SELECT schemaname,relname,n_live_tup FROM pg_stat_user_tables ORDER BY schemaname, n_live_tup DESC;")##number of rows, approximate
	if (ncol(temp)==0) return(data.frame(Error= "No table in database"))
names(temp) <- c("schema name", "table name", "number of rows")
cat(file=stderr(), paste(dim(temp), collapse=" ; "))
if(nrow(temp)>0) temp[,3] <- as.integer(temp[,3])
return(temp)
}