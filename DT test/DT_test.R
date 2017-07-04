#devtools::install_github('rstudio/DT@feature/editor')

# library(shiny)
# library(DT)
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
        
shinyApp(
  ui = fluidPage(
  	br(),
  	downloadButton("downloadBehaviorsView", "Download behaviors", icon=icon('download'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
  	downloadButton("downloadScansView", "Download scans", icon=icon('download'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
  	HTML("<h3><b>    Sessions</b></h3>"),
	br(),
    d3tfOutput('sessionsDT', height = "auto"),
	br(),
	actionButton('duplicateSessionRow', 'Duplicate selected row', icon=icon('copy'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
	#actionButton('addBlankSessionRow', 'Add blank row', icon=icon('plus'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
	actionButton('deleteSessionRow', 'Delete selected row', icon=icon('close'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
    hr(),
    hr(),
    HTML("<h3><b>    Focal samples</b></h3>"),
	br(),
    d3tfOutput('focalsDT', height = "auto"),
	br(),
	actionButton('duplicateFocalRow', 'Duplicate selected row', icon=icon('copy'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
	#actionButton('addBlankSessionRow', 'Add blank row', icon=icon('plus'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
	actionButton('deleteFocalRow', 'Delete selected row', icon=icon('close'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
    hr(),
    hr(),
    HTML("<h3><b>    Dyadic and self-directed/health data</b></h3>"),
    br(),    
    d3tfOutput('behaviorsDT', height = "auto"),
	br(),  
	actionButton('duplicateBehaviorRow', 'Duplicate selected row', icon=icon('copy'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
	#actionButton('addBlankSessionRow', 'Add blank row', icon=icon('plus'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
	actionButton('deleteBehaviorRow', 'Delete selected row', icon=icon('close'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),  
    hr(),
    hr(),
    HTML("<h3><b>    Scan list</b></h3>"),
	br(),    
    d3tfOutput('scanListDT', height = "auto"),
    br(),    
    actionButton('duplicateScanListRow', 'Duplicate selected row', icon=icon('copy'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
	#actionButton('addBlankSessionRow', 'Add blank row', icon=icon('plus'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
	actionButton('deleteScanListRow', 'Delete selected row', icon=icon('close'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
    hr(),
    hr(),
    HTML("<h3><b>    Scan data</b></h3>"),
	br(), 
    d3tfOutput('scansDT', height = "auto"),
    br(),
    actionButton('duplicateScanRow', 'Duplicate selected row', icon=icon('copy'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
	#actionButton('addBlankSessionRow', 'Add blank row', icon=icon('plus'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),
	actionButton('deleteScanRow', 'Delete selected row', icon=icon('close'), style="color: #090909; background-color: #cdcdcd; border-color: #090909"),    
    hr(),
    hr()
  ),
  
  server <- function(input, output, session) { 
    views <- reactiveValues(dat1=dat1, dat2=dat2)

    ##function to remove duplicated values
    removeDuplicates <- function(dat, vec){
    	temp <- dat[,match(vec, names(dat))]
    	return(temp[!duplicated(temp),])
    }
    ###########################
    sessionsRV <-  reactive({
    	#temp <- is.null(input$addBlankSessionRow)+clicked$addBlankSessionRow
    	if (is.null(isolate(views$dat1))) return(NULL)##why did I isolate this value?
    	temp <- is.null(input$sessionsDT_edit) & is.null(clicked$deleteSession)
    	res <- removeDuplicates(isolate(views$dat1), c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "layout_info_json_version", "behaviors_json_version", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold"))
    	cat(file=stderr(), paste0("sessionsRV updated : ", nrow(res), " nrow(views$dat1) = ", nrow(views$dat1), "\n"))
    return(res)
    	})
    ###########################
    emptySessionRow <- function(){
    	sessionColnames <- c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "layout_info_json_version", "behaviors_json_version", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold")
    	dat <- data.frame(matrix(NA,nrow=1, ncol=length(sessionColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- sessionColnames
    	return(dat)
    	}
	sessionsRVentry <- reactiveValues(dat=emptySessionRow())
    ###########################

    clicked <- reactiveValues(deleteSession=FALSE, focalsDTRowSelected =FALSE)


	focalsRV <- reactive({
		if(is.null(input$sessionsDT_select)) return(NULL)##checks if a sessionsDT row has been selected
		temp <- is.null(input$focalsDT_edit)
		res <- isolate(removeDuplicates(views$dat1[views$dat1$device_id==sessionsRV()$device_id[input$sessionsDT_select] & views$dat1$session_start_time==sessionsRV()$session_start_time[input$sessionsDT_select],],c("focal_start_time", "focal_end_time", "focal_individual_id", "set_duration", "set_scan_interval")))
		cat(file=stderr(), paste0("focalsRV updated with sessionsDT_select = ",input$sessionsDT_select," and ", res[1,1], " and clicked$focalsDTRowSelected = ", clicked$focalsDTRowSelected, "\n\n"))
		return(res)
	})
	
	behaviorsRV <- reactive({
		if(is.null(input$sessionsDT_select) | is.null(input$focalsDT_select)) return(NULL)
				temp <- is.null(input$behaviorDT_edit)
		return(isolate(removeDuplicates(views$dat1[views$dat1$device_id==sessionsRV()$device_id[input$sessionsDT_select] & views$dat1$focal_start_time==focalsRV()$focal_start_time[input$focalsDT_select],],c("behavior_time", "actor", "subject", names(views$dat1)[!names(views$dat1)%in%c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "focal_start_time", "focal_end_time","focal_individual_id", "behavior_time", "actor","subject","gps_on", "compass_on", "map_mode_on", "physical_contact_threshold","layout_info_json_version" , "behaviors_json_version", "set_duration", "set_scan_interval")]))))
	})
	
	scanListRV <- reactive({
		if(is.null(input$sessionsDT_select) | is.null(input$focalsDT_select)) return(NULL)
				temp <- is.null(input$scanListDT_edit)
		return(isolate(removeDuplicates(views$dat2[views$dat2$device_id==sessionsRV()$device_id[input$sessionsDT_select] & views$dat2$focal_start_time==focalsRV()$focal_start_time[input$focalsDT_select],],c("scan_time", "latitude", "longitude", "gps_horizontal_precision", "altitude")))	)
	})

	scansRV <- reactive({
		if(is.null(input$scanListDT_select)) return(NULL)
		temp <- is.null(input$scansDT_edit)
		res <- isolate(removeDuplicates(views$dat2[views$dat2$device_id==sessionsRV()$device_id[input$sessionsDT_select] & views$dat2$scan_time== scanListRV()$scan_time[input$scanListDT_select],],c("scanned_individual_id", names(views$dat2)[!names(views$dat2)%in%c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "focal_start_time", "focal_end_time","focal_individual_id", "scan_time", "scanned_individual_id","scan_time", "latitude", "longitude", "gps_horizontal_precision", "altitude", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold","layout_info_json_version" , "behaviors_json_version", "set_duration", "set_scan_interval")])))
		res <- res[!is.na(res$scanned_individual_id),]
		return(res)
	})



    ###########################

	output$sessionsDT <- renderD3tf({
		temp <- is.null(input$deleteSessionRow)###makes function reactive to deletion
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(sessionsRV())
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
 
 
	observeEvent(input$sessionsDT_select, {
		     cat(file=stderr(), paste0("is.null(input$sessionsDT_select) = ", is.null(input$sessionsDT_select), "\n"))
	if(is.null(input$sessionsDT_select)) output$focalDT <- NULL
	output$focalsDT <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(focalsRV()))
    ));
    d3tf(isolate(focalsRV()),
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
	})    	    
    	  
	observeEvent(input$focalsDT_select, {
			if(is.null(input$focalsDT_select)) output$behaviorsDT <- NULL

		output$behaviorsDT <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(behaviorsRV()))
    ));
    d3tf(isolate(behaviorsRV()),
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
  output$scanListDT <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(scanListRV()))
    ));
    d3tf(isolate(scanListRV()),
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
	})      	  
 
	observeEvent(input$scanListDT_select, {
	output$scansDT <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(scansRV()))
    ));
    d3tf(isolate(scansRV()),
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
	}) 
    		
    		
###########################
#########################
     
  observeEvent(input$sessionsDT_edit,{
    if(is.null(input$sessionsDT_edit)) return(NULL);
     edit <- input$sessionsDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      pk1 <- sessionsRV()$device_id[row]
      pk2 <- sessionsRV()$session_start_time[row]
      colname <- names(sessionsRV())[col]
      cat(file=stderr(), "editing... row = ", row, " col = ", col, " val = ", val, "\n")
      if(col == 4) {
        oldval <- sessionsRV()[row,col];
        if(!val%in%c("PAB", "MSK", "ISA")) {
          rejectEdit(session, tbl = "sessionsDT", row = row, col = col,  id = id, value = oldval);
           return(NULL);
         }
         }      
       confirmEdit(session, tbl = "sessionsDT", row = row, col = col, id = id, value = val);
       views$dat1[views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2, names(views$dat1)==colname] <- val
       views$dat2[views$dat2$device_id==pk1 & views$dat2$session_start_time==pk2, names(views$dat2)==colname] <- val
     })
  })
  
  
    observeEvent(input$focalsDT_edit,{
    if(is.null(input$focalsDT_edit)) return(NULL);
     edit <- input$focalsDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- focalsRV()$focal_start_time[row]
      colname <- names(focalsRV())[col]
      cat(file=stderr(), "editing... row = ", row, " col = ", col, " val = ", val, "nrows = ",sum(views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2), "\n")
            cat(file=stderr(), "pk1 = ", pk1, " pk2 = ", pk2, " colname = ", colname, "\n")    
       confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
       views$dat1[views$dat1$device_id==pk1 & views$dat1$focal_start_time==pk2, names(views$dat1)==colname] <- val
       views$dat2[views$dat2$device_id==pk1 & views$dat2$focal_start_time==pk2, names(views$dat2)==colname] <- val
     })
  })

       observeEvent(input$behaviorsDT_edit,{
    if(is.null(input$behaviorsDT_edit)) return(NULL);
     edit <- input$behaviorsDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- behaviorsRV()$behavior_time[row]
      pk3 <- behaviorsRV()$actor[row]
      pk4 <- behaviorsRV()$subject[row]
      colname <- names(behaviorsRV())[col]
      cat(file=stderr(), "editing... row = ", row, " col = ", col, " val = ", val, "nrows = ",sum(views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2), "\n")
            cat(file=stderr(), "pk1 = ", pk1, " pk2 = ", pk2, " colname = ", colname, "\n")    
       confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
       views$dat1[views$dat1$device_id==pk1 & views$dat1$behavior_time==pk2 & views$dat1$actor==pk3 & views$dat1$subject==pk4, names(views$dat1)==colname] <- val
     })
  }) 
         
         
     observeEvent(input$scanListDT_edit,{
    if(is.null(input$scanListDT_edit)) return(NULL);
     edit <- input$scanListDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_time[row]
      colname <- names(scanListRV())[col]
       confirmEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value = val);
      views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & !is.na(views$dat2$scan_time), names(views$dat2)==colname] <- val
     })
  }) 
    
     observeEvent(input$scansDT_edit,{
    if(is.null(input$scansDT_edit)) return(NULL);
     edit <- input$scansDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_time[input$scanListDT_select]
      pk3 <- scansRV()$scanned_individual_id[row]
      colname <- names(scansRV())[col]
      cat(file=stderr(), paste0("pk3 = ", pk3, "\n"))
       confirmEdit(session, tbl = "scansDT", row = row, col = col, id = id, value = val);
      views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & views$dat2$scanned_individual_id==pk3 & !is.na(views$dat2$scan_time), names(views$dat2)==colname] <- val
     })
  }) 
    


    #########################   
	observeEvent(input$deleteSessionRow, {
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_time[input$sessionsDT_select]
      cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2, " nb rows to delete = ", sum(views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2), "\n"))
      views$dat1 <- views$dat1[!(views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2),]
      views$dat2 <- views$dat2[!(views$dat2$device_id==pk1 & views$dat2$session_start_time==pk2),]
      clicked$deleteSession <- !clicked$deleteSession
      output$focalDT <- NULL
	})   
    
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



