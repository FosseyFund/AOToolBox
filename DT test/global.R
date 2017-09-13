install.load <- function(package.name)
{
if (!require(package.name, character.only=T)) install.packages(package.name)
library(package.name, character.only=T)
}
install.load("rjson")
install.load("visNetwork")
install.load("shiny")
install.load("rhandsontable")
install.load("markdown")
install.load("RPostgreSQL")
install.load("gtools")

library(shiny)
source("../jsonOutputConversion.R")
options(shiny.maxRequestSize=20*1024^2)

#con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname = "aotest2", host = "localhost", port = 5432, user = "postgres", password = "postgres")

library(htmlwidgets)
library(D3TableFilter)
 timeToChar <- function(x){
  		temp <- format(x)
  		temp[grepl("NA", temp)] <- NA
  		return(temp)
  	}
# dat1 <- data.frame(dbGetQuery(con, "select * from main_tables.all_focal_data_view;"))
# dat1[,unlist(lapply(dat1, function(x) inherits(x, "POSIXt")))] <- apply(dat1[,unlist(lapply(dat1, function(x) inherits(x, "POSIXt")))], 2, timeToChar)
# dat2 <- data.frame(dbGetQuery(con, "select * from main_tables.all_scan_data_view;"))