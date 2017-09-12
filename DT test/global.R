library(DT)
library(shiny)
library(RPostgreSQL)
source("../jsonOutputConversion.R")
options(shiny.maxRequestSize=20*1024^2)

con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname = "aotest2", host = "localhost", port = 5432, user = "postgres", password = "postgres")

library(htmlwidgets)
library(D3TableFilter)
 timeToChar <- function(x){
  		temp <- format(x)
  		temp[grepl("NA", temp)] <- NA
  		return(temp)
  	}
dat1 <- data.frame(dbGetQuery(con, "select * from main_tables.all_focal_data_view;"))
dat1[,unlist(lapply(dat1, function(x) inherits(x, "POSIXt")))] <- apply(dat1[,unlist(lapply(dat1, function(x) inherits(x, "POSIXt")))], 2, timeToChar)
dat2 <- data.frame(dbGetQuery(con, "select * from main_tables.all_scan_data_view;"))