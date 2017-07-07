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
    
    ###########################
    emptySessionRow <- function(){
    	sessionColnames <- c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "layout_info_json_version", "behaviors_json_version", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold")
    	dat <- data.frame(matrix(NA,nrow=1, ncol=length(sessionColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- sessionColnames
    	return(dat)
    	}
    	
    	emptyFocalListRow <- function(){
    focalListColnames <- c("focal_start_time", "focal_end_time", "focal_individual_id", "set_duration", "set_scan_interval")
    	dat <- data.frame(matrix("",nrow=1, ncol=length(focalListColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- focalListColnames
    	return(dat)
    	}
    	
    	
    emptyBehaviorRow <- function(){
    behaviorColnames <- c("behavior_time", "actor", "subject", names(views$dat1)[!names(views$dat1)%in%c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "focal_start_time", "focal_end_time","focal_individual_id", "behavior_time", "actor","subject","gps_on", "compass_on", "map_mode_on", "physical_contact_threshold","layout_info_json_version" , "behaviors_json_version", "set_duration", "set_scan_interval")])
    	dat <- data.frame(matrix("",nrow=1, ncol=length(behaviorColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- behaviorColnames
    	return(dat)
    	}
    	
    	emptyScanListRow <- function(){
    scanListColnames <- c("scan_time", "latitude", "longitude", "gps_horizontal_precision", "altitude")
    	dat <- data.frame(matrix("",nrow=1, ncol=length(scanListColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- scanListColnames
    	return(dat)
    	}


    emptyScanRow <- function(){
    scanColnames <- c("scanned_individual_id", names(views$dat2)[!names(views$dat2)%in%c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "focal_start_time", "focal_end_time","focal_individual_id", "scan_time", "scanned_individual_id","scan_time", "latitude", "longitude", "gps_horizontal_precision", "altitude", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold","layout_info_json_version" , "behaviors_json_version", "set_duration", "set_scan_interval")])
    	dat <- data.frame(matrix("",nrow=1, ncol=length(scanColnames)))
    	for(i in 1:ncol(dat))  dat[,i] <- as.character(dat[,i])
    	names(dat) <- scanColnames
    	return(dat)
    	}
    
	sessionsRVentry <- reactiveValues(dat=emptySessionRow())
    ###########################

    clicked <- reactiveValues(deleteSession=FALSE)

	sessionsRV <-  reactive({
    	#temp <- is.null(input$addBlankSessionRow)+clicked$addBlankSessionRow
    	if (is.null((views$dat1))) return(emptySessionRow())##why did I isolate this value?
    	#temp <- is.null(input$sessionsDT_edit) & is.null(clicked$deleteSession)
    	res <- removeDuplicates((views$dat1), c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "layout_info_json_version", "behaviors_json_version", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold"))
    	cat(file=stderr(), paste0("sessionsRV updated : ", nrow(res), " nrow(views$dat1) = ", nrow(views$dat1), "\n"))
    return(res)
    	})

	focalsRV <- reactive({
		if(isolate(is.null(input$sessionsDT_select))) return(emptyFocalListRow())##checks if a sessionsDT row has been selected
		#temp <- is.null(input$focalsDT_edit)
		res <-(removeDuplicates(views$dat1[views$dat1$device_id==sessionsRV()$device_id[input$sessionsDT_select] & views$dat1$session_start_time==sessionsRV()$session_start_time[input$sessionsDT_select],],c("focal_start_time", "focal_end_time", "focal_individual_id", "set_duration", "set_scan_interval")))
		cat(file=stderr(), paste0("focalsRV updated with sessionsDT_select = ",isolate(input$sessionsDT_select)," and ", res[1,1], " and input$focalsDT_select = ", isolate(input$focalsDT_select),"\n\n"))

		return(res)
	})
	
	behaviorsRV <- reactive({
		if((is.null(input$sessionsDT_select) | is.null(input$focalsDT_select))) return(emptyBehaviorRow())
		#temp <- is.null(input$behaviorDT_edit)
		cat(file=stderr(), paste0("behaviorsRV updated with sessionsDT_select = ",input$sessionsDT_select," and input$focalsDT_select = ", input$focalsDT_select, "\n\n"))
		return((removeDuplicates(views$dat1[views$dat1$device_id==sessionsRV()$device_id[input$sessionsDT_select] & views$dat1$focal_start_time==focalsRV()$focal_start_time[input$focalsDT_select] & views$dat1$session_start_time==sessionsRV()$session_start_time[input$sessionsDT_select],],c("behavior_time", "actor", "subject", names(views$dat1)[!names(views$dat1)%in%c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "focal_start_time", "focal_end_time","focal_individual_id", "behavior_time", "actor","subject","gps_on", "compass_on", "map_mode_on", "physical_contact_threshold","layout_info_json_version" , "behaviors_json_version", "set_duration", "set_scan_interval")]))))
	})
	
	scanListRV <- reactive({
		if((is.null(input$sessionsDT_select) | is.null(input$focalsDT_select))) return(emptyScanListRow())
				#temp <- is.null(input$scanListDT_edit)
		return((removeDuplicates(views$dat2[views$dat2$device_id==sessionsRV()$device_id[isolate(input$sessionsDT_select)] & views$dat2$session_start_time==sessionsRV()$session_start_time[input$sessionsDT_select] & views$dat2$focal_start_time==focalsRV()$focal_start_time[isolate(input$focalsDT_select)],],c("scan_time", "latitude", "longitude", "gps_horizontal_precision", "altitude")))	)
	})

	scansRV <- reactive({
		if((is.null(input$scanListDT_select))) return(emptyScanRow())
		#temp <- is.null(input$scansDT_edit)
		
		res <-(removeDuplicates(views$dat2[views$dat2$device_id==sessionsRV()$device_id[isolate(input$sessionsDT_select)] & views$dat2$session_start_time==sessionsRV()$session_start_time[input$sessionsDT_select] & views$dat2$scan_time== scanListRV()$scan_time[isolate(input$scanListDT_select)] & !is.na(views$dat2$scan_time),],c("scanned_individual_id", names(views$dat2)[!names(views$dat2)%in%c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "focal_start_time", "focal_end_time","focal_individual_id", "scan_time", "scanned_individual_id","scan_time", "latitude", "longitude", "gps_horizontal_precision", "altitude", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold","layout_info_json_version" , "behaviors_json_version", "set_duration", "set_scan_interval")])))
		return(res)
	})



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

 
	observeEvent(input$sessionsDT_select, {
		     cat(file=stderr(), paste0("is.null(input$sessionsDT_select) = ", is.null(input$sessionsDT_select), "\n"))
	output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDT", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyFocalListRow())));
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
  
  
	output$behaviorsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render behaviorsDT", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyBehaviorRow()))
    ));
    d3tf(emptyBehaviorRow(),
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
								     cat(file=stderr(), paste0("render scanListsDT", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanListRow()))
    ));
    d3tf(emptyScanListRow(),
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

	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDT", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(emptyScanRow(),
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
			#if(is.null(input$focalsDT_select)) output$behaviorsDT <- NULL

		output$behaviorsDT <- renderD3tf({
									     cat(file=stderr(), paste0("render behaviorsDTbis", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyBehaviorRow()))
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
  							     cat(file=stderr(), paste0("render scanListDTbis", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanListRow()))
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
  
  output$scansDT <- renderD3tf({
  	  							     cat(file=stderr(), paste0("render scansDTbis", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(isolate(emptyScanRow()),
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
		  							     cat(file=stderr(), paste0("render scansDTter", "\n"))
cat(file=stderr(), paste0("scansRV dim : ", dim(scansRV()), "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
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
###########################
#################edit cells
###########################
###########################

     
  observeEvent(input$sessionsDT_edit,{
    if(is.null(input$sessionsDT_edit)) return(NULL);
    cat(file=stderr(), paste0("input$sessionsDT_edit with row: ", input$sessionsDT_select, "\n"))
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
    
       views$dat1[views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2, names(views$dat1)==colname] <- val
       views$dat2[views$dat2$device_id==pk1 & views$dat2$session_start_time==pk2, names(views$dat2)==colname] <- val
        ##confirmEdit(session, tbl = "sessionsDT", row = row, col = col, id = id, value = val)
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
      pk3 <- sessionsRV()$session_start_time[input$sessionsDT_select]

      colname <- names(focalsRV())[col]
      #cat(file=stderr(), "editing... row = ", row, " col = ", col, " val = ", val, " pk1 = ", pk1, "pk2 = ",pk2 ,"\n")
       if(is.na(pk2)){
       	views$dat1[views$dat1$device_id==pk1 & views$dat1$session_start_time ==pk3, names(views$dat1)==colname] <- val
       	views$dat2[views$dat2$device_id==pk1 & views$dat2$session_start_time ==pk3, names(views$dat2)==colname] <- val
       } else {
        
       views$dat1[views$dat1$device_id==pk1 & views$dat1$focal_start_time==pk2, names(views$dat1)==colname] <- val
       views$dat2[views$dat2$device_id==pk1 & views$dat2$focal_start_time==pk2, names(views$dat2)==colname] <- val
              #confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
        }
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
      pk5 <- sessionsRV()$session_start_time[input$sessionsDT_select]
      pk6 <- focalsRV()$focal_start_time[input$focalsDT_select]

      if(is.null(input$focalsDT_select)) {
	       rejectEdit(session, tbl = "behaviorsDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(behaviorsRV())[col]          
        if(is.na(pk2) | is.na(pk3) | is.na(pk4)){
       	views$dat1[views$dat1$device_id==pk1 & views$dat1$session_start_time ==pk5 & views$dat1$focal_start_time==pk6, names(views$dat1)==colname] <- val
       } else {
       #confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
       views$dat1[views$dat1$device_id==pk1 & views$dat1$behavior_time==pk2 & views$dat1$actor==pk3 & views$dat1$subject==pk4, names(views$dat1)==colname] <- val
       }
     }})
  }) 
         
         
      observeEvent(input$scanListDT_edit,{
    if(is.null(input$scanListDT_edit)) return(NULL);
     edit <- input$scanListDT_edit;
isolate({
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_time[row]
      pk3 <- sessionsRV()$session_start_time[input$sessionsDT_select]
	  #oldval <- scanListRV()[row, col]
if(is.null(input$focalsDT_select)) {
	       rejectEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value = "");
} else {

      colname <- names(scanListRV())[col]      
      if(is.na(pk2)){
       	views$dat2[views$dat2$device_id==pk1 & views$dat2$session_start_time ==pk3, names(views$dat2)==colname] <- val
       } else {
       
       #cat(file=stderr(), "editing... row = ", row, " col = ", col, " val = ", val, " pk1 = ", pk1, "pk2 = ",pk2 ,"pk3= ",pk3 , "colname = ", colname,"oldval = ",oldval, "\n")
       #confirmEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value = val);
     
      #if (colname=="scan_time") pk2 <- oldval
      views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & !is.na(views$dat2$scan_time), names(views$dat2)==colname] <- val
      }      
     }
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
      cat(file=stderr(), paste0("pk1 = ", pk1,"pk2 = ", pk2,"pk3 = ", pk3, "\n"))

 if(is.null(input$scanListDT_select)) {
	       rejectEdit(session, tbl = "scansDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(scansRV())[col]
            if(is.na(pk3)){
        cat(file=stderr(), paste0("pk3 is NA, pk1 = ", pk1,"; pk2 = ", pk2,"; pk3 = ", pk3, "\n")) 
        cat(file=stderr(), paste0("nb rows from dat2 to edit : ", sum(views$dat2$device_id==pk1 & views$dat2$scan_time ==pk2), "\n")) 
       	views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time ==pk2 & !is.na(views$dat2$scan_time), names(views$dat2)==colname] <- val
       } else {
      cat(file=stderr(), paste0("pk3 = ", pk3, "\n"))
       #confirmEdit(session, tbl = "scansDT", row = row, col = col, id = id, value = val);
      views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & views$dat2$scanned_individual_id==pk3 & !is.na(views$dat2$scan_time), names(views$dat2)==colname] <- val
     }}})
  }) 
    

 #########################   
 #########################   
 ######row deletion
 #########################   
 #########################
  
	observeEvent(input$deleteSessionRow, {
		if(!is.null(input$sessionsDT_select)) {
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_time[input$sessionsDT_select]
      cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2, " nb rows to delete = ", sum(views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2), "\n"))
      views$dat1 <- views$dat1[!(views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2),]
      views$dat2 <- views$dat2[!(views$dat2$device_id==pk1 & views$dat2$session_start_time==pk2),]
      #clicked$deleteSession <- !clicked$deleteSession
      #output$focalDT <- NULL
      
      output$sessionsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render sessionsDTDelete", "\n"))

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
      
      output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDTDelete", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyFocalListRow())));
    d3tf(isolate(emptyFocalListRow()),
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
  
  
	output$behaviorsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render behaviorsDTDelete", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyBehaviorRow()))
    ));
    d3tf(emptyBehaviorRow(),
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
								     cat(file=stderr(), paste0("render scanListsDTDelete", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanListRow()))
    ));
    d3tf(emptyScanListRow(),
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

	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDelete", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(emptyScanRow(),
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
      
      
      }
	})   
    
    	observeEvent(input$deleteFocalRow, {
    	cat(file=stderr(), paste0("deleting focal row : ", input$focalsDT_select, "\n"))
    		
		if(!is.null(input$focalsDT_select)) {
			
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- focalsRV()$focal_start_time[input$focalsDT_select]
      
      cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2, " nb rows to delete = ",sum(views$dat1$device_id==pk1 & views$dat1$focal_start_time==pk2), "\n"))

       views$dat1 <- views$dat1[!(views$dat1$device_id==pk1 & views$dat1$focal_start_time==pk2),]
       views$dat2[!(views$dat2$device_id==pk1 & views$dat2$focal_start_time==pk2),]
      
      
      	output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDTDelete", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyFocalListRow())));
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
      
     output$behaviorsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render behaviorsDTDelete", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyBehaviorRow()))
    ));
    d3tf(emptyBehaviorRow(),
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
								     cat(file=stderr(), paste0("render scanListsDTDelete", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanListRow()))
    ));
    d3tf(emptyScanListRow(),
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

	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDelete", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(emptyScanRow(),
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
      }
	})  
    
    
    observeEvent(input$deleteBehaviorRow, {
    	    	cat(file=stderr(), paste0("deleting behavior row : ", input$behaviorsDT_select, "\n"))
	if(!is.null(input$behaviorsDT_select)) {
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]		
      pk2 <- behaviorsRV()$behavior_time[input$behaviorsDT_select]
      pk3 <- behaviorsRV()$actor[input$behaviorsDT_select]
      pk4 <- behaviorsRV()$subject[input$behaviorsDT_select]
      focalsDTSelectedRow <- input$focalsDT_select
       views$dat1 <- views$dat1[!(views$dat1$device_id==pk1 & views$dat1$behavior_time==pk2 & views$dat1$actor==pk3 & views$dat1$subject==pk4),]
      #setRowClass(session, tbl="focalsDT", row= focalsDTSelectedRow, class="active")
      cat(file=stderr(), paste0("set row class for focalsDT row : ", focalsDTSelectedRow, "\n"))

      
      }
	})
	
	
	
	    	observeEvent(input$deleteScanListRow, {
    	cat(file=stderr(), paste0("deleting scanListDT row : ", input$scanListDT_select, "\n"))
    		
		if(!is.null(input$scanListDT_select)) {
			
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_time[input$scanListDT_select]
      
      cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2, " nb rows to delete = ",sum(views$dat2$device_id==pk1 & views$dat2$scan_time ==pk2 & !is.na(views$dat2$scan_time)), "\n"))

       views$dat2 <- views$dat2[!(views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & !is.na(views$dat2$scan_time)),]      
      
      	output$scanListDT <- renderD3tf({
				     cat(file=stderr(), paste0("render scanListDTDelete", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyScanListRow())));
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
      
     
	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDelete", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(emptyScanRow(),
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
      }
	})  

	    	observeEvent(input$deleteScanRow, {
    	cat(file=stderr(), paste0("deleting scansDT row : ", input$scansDT_select, "\n"))
    		
		if(!is.null(input$scansDT_select)) {
			
 	  pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_time[input$scanListDT_select]
      pk3 <- scansRV()$scanned_individual_id[input$scansDT_select]
            
      cat(file=stderr(), paste0("pk1 = ", pk1," pk2 = ", pk2," pk3 = ", pk3, " nb rows to delete = ", sum(views$dat2$device_id==pk1 & views$dat2$scan_time ==pk2 & views$dat2$scanned_individual_id ==pk3 & !is.na(views$dat2$scan_time)), "\n"))

       views$dat2 <- views$dat2[!(views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & views$dat2$scanned_individual_id==pk3 & !is.na(views$dat2$scan_time)),]      
      
    output$scansDT <- renderD3tf({
				     cat(file=stderr(), paste0("render scansDTDelete", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyScanRow())));
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
    }
	})  	  

 #########################   
 #########################   
 ######row duplication
 #########################   
 #########################

 observeEvent(input$duplicateSessionRow, {
		if(!is.null(input$sessionsDT_select)) {
      cat(file=stderr(), paste0("duplicating... "))
      dupRow <- sessionsRV()[input$sessionsDT_select,]
      dupRow$session_start_time <- paste(dupRow$session_start_time, "EDIT !")
      views$dat1 <- smartbind(views$dat1, dupRow)
      views$dat2 <- smartbind(views$dat2, dupRow)

      output$sessionsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render sessionsDT RowDuplicate", "\n"))
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
  
  output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDTDuplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyFocalListRow())));
    d3tf(isolate(emptyFocalListRow()),
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
  
	output$behaviorsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render behaviorsDTDuplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyBehaviorRow()))
    ));
    d3tf(emptyBehaviorRow(),
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
								     cat(file=stderr(), paste0("render scanListsDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanListRow()))
    ));
    d3tf(emptyScanListRow(),
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

	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(emptyScanRow(),
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
  
  }
  })
 
 
 
 	observeEvent(input$duplicateFocalRow, {
    		
		if(!is.null(input$focalsDT_select)) {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[input$sessionsDT_select,]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRow <- cbind(dupRowSession, dupRowFocal)
      dupRow$focal_start_time <- paste(dupRow$focal_start_time, "EDIT !")
      dupRow$focal_end_time <- paste(dupRow$focal_end_time, "EDIT !")
      dupRow$focal_individual_id <- "ENTER FOCAL INDIV ID"

      views$dat1 <- smartbind(views$dat1, dupRow)
      views$dat2 <- smartbind(views$dat2, dupRow)
		
		
     
      	output$focalsDT <- renderD3tf({
				     cat(file=stderr(), paste0("render focalsDTDuplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(emptyFocalListRow())));
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
      
     output$behaviorsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render behaviorsDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyBehaviorRow()))
    ));
    d3tf(emptyBehaviorRow(),
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
								     cat(file=stderr(), paste0("render scanListsDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanListRow()))
    ));
    d3tf(emptyScanListRow(),
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

	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDuplicate", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(emptyScanRow(),
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
      }
	})  
 
 
 	observeEvent(input$duplicateBehaviorRow, {
    		
		if(!is.null(input$behaviorsDT_select)) {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[input$sessionsDT_select,]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowBehav <- behaviorsRV()[input$behaviorsDT_select,]

      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowBehav)
      dupRow$behavior_time <- paste(dupRow$behavior_time, "EDIT !")
      dupRow$actor <- "ENTER ACTOR"
      dupRow$subject <- "ENTER SUBJECT"

      views$dat1 <- smartbind(views$dat1, dupRow)
      
     output$behaviorsDT <- renderD3tf({
						     cat(file=stderr(), paste0("render behaviorsDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyBehaviorRow()))
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
		
      }
	})   
 
 	observeEvent(input$duplicateScanListRow, {
    		
		if(!is.null(input$scanListDT_select)) {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[input$sessionsDT_select,]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowScanList <- scanListRV()[input$scanListDT_select,]

      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList)
      dupRow$scan_time <- paste(dupRow$scan_time, "EDIT !")

      views$dat2 <- smartbind(views$dat2, dupRow)
      
     output$scanListDT <- renderD3tf({
						     cat(file=stderr(), paste0("render scanListDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanListRow()))
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
  
  	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
    ));
    d3tf(emptyScanRow(),
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
      }
	}) 
    

 	observeEvent(input$duplicateScanRow, {
    		
		if(!is.null(input$scansDT_select)) {
		
	  cat(file=stderr(), paste0("duplicating... "))
      dupRowSession <- sessionsRV()[input$sessionsDT_select,]
      dupRowFocal <- focalsRV()[input$focalsDT_select,]
      dupRowScanList <- scanListRV()[input$scanListDT_select,]
      dupRowScan <- scansRV()[input$scansDT_select,]

      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList, dupRowScan)
      dupRow$scanned_individual_id <- paste(dupRow$scanned_individual_id, "EDIT !")

      views$dat2 <- smartbind(views$dat2, dupRow)		
  
  	output$scansDT <- renderD3tf({
								     cat(file=stderr(), paste0("render scansDTDuplicate", "\n"))

    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", isolate(ncol(emptyScanRow()))
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
      }
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



