
# library(shiny)
# library(gtools)
# library(RPostgreSQL)
# con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname = "aowinnie5433", host = "localhost", port = 5433, user = "postgres", password = "postgres")

# library(htmlwidgets)
# library(D3TableFilter)
# timeToChar <- function(x){
  		# temp <- format(x)
  		# temp[grepl("NA", temp)] <- NA
  		# return(temp)
  	# }
# dat1 <- data.frame(dbGetQuery(con, "select * from main_tables.all_focal_data_view;"))
# dat1[,unlist(lapply(dat1, function(x) inherits(x, "POSIXt")))] <- apply(dat1[,unlist(lapply(dat1, function(x) inherits(x, "POSIXt")))], 2, timeToChar)
# dat2 <- data.frame(dbGetQuery(con, "select * from main_tables.all_scan_data_view;"))
# dat2[,unlist(lapply(dat2, function(x) inherits(x, "POSIXt")))] <- apply(dat2[,unlist(lapply(dat2, function(x) inherits(x, "POSIXt")))], 2, timeToChar)  
        

  
shinyServer(function(input, output, session) { 
    views <- reactiveValues(dat1=dat1, dat2=dat2)

    ##function to remove duplicated values
    removeDuplicates <- function(dat, vec){
    	temp <- dat[,match(vec, names(dat))]
    	return(temp[!duplicated(temp),])
    }

###########################
###########################
###########################
###########################
source("create_empty_tables.R", local=TRUE)
source("tablesRV.R", local=TRUE)

###########################
###########################
###########################
###########################

output$sessionsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render sessionsDT", "\n"))

	#temp <- is.null(input$deleteSessionRow)###makes function reactive to deletion
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptySessionRow())
    ));
    d3tf(isolate(sessionsRV()),
         tableProps = isolate(tableProps),
         extensions = list(
           list(name = "sort")
         ),
         showRowNames = FALSE,
         tableStyle = "table table-bordered",
         edit = TRUE,
         selectableRows='single',
         selectableRowsClass='success'
	);
  })

###########################    		
###########################
#################select rows
###########################
###########################
source("tableSelect.R", local=TRUE)
    		
###########################    		
###########################
#################edit cells
###########################
###########################
source("cellEdit.R", local=TRUE)
     
#########################   
#########################   
######row deletion
#########################   
#########################
source("rowDelete.R", local=TRUE)

#########################   
#########################   
######row duplication
#########################   
#########################
source("rowDuplicate.R", local=TRUE)


#########################   
    
    output$downloadBehaviorsView <- downloadHandler(
    filename = function() { 
		 paste('BehaviorsView.csv', sep='') 
	 },
    content = function(file) {
     write.csv(views$dat1, file, row.names=F, na="")
    }
  	)
    output$downloadScansView <- downloadHandler(
    filename = function() { 
		 paste('ScansView.csv', sep='') 
	 },
    content = function(file) {
     write.csv(views$dat2, file, row.names=F, na="")
    }
  )
  }
)



