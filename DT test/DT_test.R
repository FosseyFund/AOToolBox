#devtools::install_github('rstudio/DT@feature/editor')

# library(shiny)
# library(DT)
# con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname = "aotest", host = "localhost", port = 5432, user = "postgres", password = "postgres")


shinyApp(
  ui = fluidPage(
  	#verbatimTextOutput('sessionRawSelected'),
  	#hr(),
  	downloadButton("downloadBehaviorsView", "Download behaviors"),
  	downloadButton("downloadScansView", "Download scans"),
  	HTML("<h3><b>    Sessions</b></h3>"),
	br(),    
    DT::dataTableOutput('sessionsDT'),
	br(),    
    hr(),
    hr(),
    HTML("<h3><b>    Focal samples</b></h3>"),
	br(),    
    DT::dataTableOutput('focalsDT'),
	br(),    
    hr(),
    hr(),
    HTML("<h3><b>    Dyadic and self-directed/health data</b></h3>"),
    br(),    
    DT::dataTableOutput('behaviorsDT'),
	br(),    
    hr(),
    hr(),
    HTML("<h3><b>    Scan list</b></h3>"),
	br(),    
    DT::dataTableOutput('scanListDT'),
    br(),    
    hr(),
    hr(),
    HTML("<h3><b>    Scan data</b></h3>"),
	br(), 
    DT::dataTableOutput('scansDT'),
    br(),    
    hr(),
    hr()
  ),
  server <- function(input, output, session) {
  	timeToChar <- function(x){
  		temp <- format(x)
  		temp[grepl("NA", temp)] <- NA
  		return(temp)
  	}
    dat1 <- data.frame(dbGetQuery(con, "select * from main_tables.all_focal_data_view;"))
    dat1[,unlist(lapply(dat1, function(x) inherits(x, "POSIXt")))] <- apply(dat1[,unlist(lapply(dat1, function(x) inherits(x, "POSIXt")))], 2, timeToChar)
    dat2 <- data.frame(dbGetQuery(con, "select * from main_tables.all_scan_data_view;"))
    dat2[,unlist(lapply(dat2, function(x) inherits(x, "POSIXt")))] <- apply(dat2[,unlist(lapply(dat2, function(x) inherits(x, "POSIXt")))], 2, timeToChar)    
    
    views <- reactiveValues(dat1=dat1, dat2=dat2)

    ##function to remove duplicated values
    removeDuplicates <- function(dat, vec){
    	temp <- dat[,match(vec, names(dat))]
    	return(temp[!duplicated(temp),])
    }
    ###########################
    sessionsRV <-  reactive({    	
    			cat(file=stderr(), paste0("sessionsRV updated ","\n"))
    	if (is.null(isolate(views$dat1))) return(NULL)
    return(removeDuplicates(isolate(views$dat1), c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "layout_info_json_version", "behaviors_json_version", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold")))
    	})


	focalsRV <- reactive({
		    			cat(file=stderr(), paste0("focalsRV updated", "\n\n"))

		if(is.null(input$sessionsDT_rows_selected)) return(NULL)
						res <- isolate(removeDuplicates(views$dat1[views$dat1$device_id==sessionsRV()$device_id[input$sessionsDT_rows_selected] & views$dat1$session_start_time==sessionsRV()$session_start_time[input$sessionsDT_rows_selected],],c("focal_start_time", "focal_end_time", "focal_individual_id", "set_duration", "set_scan_interval")))
		return(res)
	# cat(file=stderr(), paste0("sessionsRV()$session_start_time[input$sessionsDT_rows_selected] = ", sessionsRV()$session_start_time[input$sessionsDT_rows_selected], "\n"))##old value
	# behaviorsRV$dat <- NULL
	# scanListRV$dat <-  NULL
	# scansRV$dat <- NULL
	})
	
	behaviorsRV <- reactive({
		    			#cat(file=stderr(), paste0("behaviorsRV updated", "\n"))
		#cat(file=stderr(), paste0("input$focalsDT_rows_selected = ", input$focalsDT_rows_selected, "\n"))
		if(is.null(input$sessionsDT_rows_selected) | is.null(input$focalsDT_rows_selected)) return(NULL)
		return(isolate(removeDuplicates(views$dat1[views$dat1$device_id==sessionsRV()$device_id[input$sessionsDT_rows_selected] & views$dat1$focal_start_time==focalsRV()$focal_start_time[input$focalsDT_rows_selected],],c("behavior_time", "actor", "subject", names(views$dat1)[!names(views$dat1)%in%c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "focal_start_time", "focal_end_time","focal_individual_id", "behavior_time", "actor","subject","gps_on", "compass_on", "map_mode_on", "physical_contact_threshold","layout_info_json_version" , "behaviors_json_version", "set_duration", "set_scan_interval")])))	)
		#cat(file=stderr(), paste0("device_id = ", sessionsRV()$device_id[input$sessionsDT_rows_selected], "\nfocal_start_time = ", focalsRV$dat$focal_start_time[input$focalsDT_rows_selected], "\n"))
	})
	
	scanListRV <- reactive({
		#cat(file=stderr(), paste0("scanListRV updated", "\n"))
		if(is.null(input$sessionsDT_rows_selected) | is.null(input$focalsDT_rows_selected)) return(NULL)
		return(isolate(removeDuplicates(views$dat2[views$dat2$device_id==sessionsRV()$device_id[input$sessionsDT_rows_selected] & views$dat2$focal_start_time==focalsRV()$focal_start_time[input$focalsDT_rows_selected],],c("scan_time", "latitude", "longitude", "gps_horizontal_precision", "altitude")))	)
		#cat(file=stderr(), paste0("device_id = ", sessionsRV()$device_id[input$sessionsDT_rows_selected], "\nfocal_start_time = ", focalsRV$dat$focal_start_time[input$focalsDT_rows_selected], "\n"))
	})

	scansRV <- reactive({
				#cat(file=stderr(), paste0("scansRV updated", "\n"))

		if(is.null(input$scanListDT_rows_selected)) return(NULL)
		res <- isolate(removeDuplicates(views$dat2[views$dat2$device_id==sessionsRV()$device_id[input$sessionsDT_rows_selected] & views$dat2$scan_time== scanListRV()$scan_time[input$scanListDT_rows_selected],],c("scanned_individual_id", names(views$dat2)[!names(views$dat2)%in%c("device_id", "session_start_time", "session_end_time", "group_id", "pin_code_name", "focal_start_time", "focal_end_time","focal_individual_id", "scan_time", "scanned_individual_id","scan_time", "latitude", "longitude", "gps_horizontal_precision", "altitude", "gps_on", "compass_on", "map_mode_on", "physical_contact_threshold","layout_info_json_version" , "behaviors_json_version", "set_duration", "set_scan_interval")])))
		res <- res[!is.na(res$scanned_individual_id),]
		return(res)
		#cat(file=stderr(), paste0("device_id = ", sessionsRV()$device_id[input$sessionsDT_rows_selected], "\nfocal_start_time = ", focalsRV$dat$focal_start_time[input$focalsDT_rows_selected], "\n"))
	})



    ###########################
    output$sessionsDT <- DT::renderDataTable(isolate(sessionsRV()), selection = 'single', server=FALSE, rownames=TRUE, options = list(
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#C00', 'color': '#fff'});",
    "}")
	))
    	    
	observeEvent(input$sessionsDT_rows_selected, {
	output$focalsDT <- DT::renderDataTable(focalsRV(), selection = "single", server=FALSE, rownames= TRUE, options = list(
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#40F', 'color': '#fff'});",
    "}")
	))
	})    	    
    	  
	observeEvent(input$focalsDT_rows_selected, {
		#cat(file=stderr(), paste0("2input$focalsDT_rows_selected = ", input$focalsDT_rows_selected, "\n"))
	output$behaviorsDT <- DT::renderDataTable(behaviorsRV(), selection = "single", server=FALSE, rownames= TRUE, options = list(
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#40F', 'color': '#fff'});",
    "}")
	))
	output$scanListDT <- DT::renderDataTable(scanListRV(), selection = "single", server=FALSE, rownames= TRUE, options = list(
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#709', 'color': '#fff'});",
    "}")
))
	})      	  
 
	observeEvent(input$scanListDT_rows_selected, {
	output$scansDT <- DT::renderDataTable(scansRV(), selection = "single", server=FALSE, rownames= TRUE, options = list(
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#74D', 'color': '#fff'});",
    "}")
	))
	}) 
    		
    		
    		
    output$sessionRawSelected <- renderPrint(input$sessionsDT_rows_selected)	
    ###########################
    #proxy <- dataTableProxy('sessionsDT')
#########################
    observeEvent(input$sessionsDT_cell_edit, {
      info = input$sessionsDT_cell_edit
     # str(info)
      i = info$row
      j = info$col
      v = info$value
      pk1 <- sessionsRV()$device_id[i]
      pk2 <- sessionsRV()$session_start_time[i]
      colname <- names(sessionsRV())[j]
       #cat(file=stderr(), paste0("val = ", sessionsRV()$group_id[i], "\n"))##old value
      #cat(file=stderr(), paste0("pk1 = ", pk1, "\npk2 = ", pk2, "\n", "col = ", names(sessionsRV())[j], "\n"))
      views$dat1[views$dat1$device_id==pk1 & views$dat1$session_start_time==pk2, names(views$dat1)==colname] <- v
      views$dat2[views$dat2$device_id==pk1 & views$dat2$session_start_time==pk2, names(views$dat2)==colname] <- v
  
    })
    
    observeEvent(input$focalsDT_cell_edit, {
      info = input$focalsDT_cell_edit
     # str(info)
      i = info$row
      j = info$col
      v = info$value
      pk1 <- sessionsRV()$device_id[input$sessionsDT_rows_selected]
      pk2 <- focalsRV()$focal_start_time[i]
      colname <- names(focalsRV())[j]
      views$dat1[views$dat1$device_id==pk1 & views$dat1$focal_start_time==pk2, names(views$dat1)==colname] <- v
      views$dat2[views$dat2$device_id==pk1 & views$dat2$focal_start_time==pk2, names(views$dat2)==colname] <- v
    })
    
      observeEvent(input$behaviorsDT_cell_edit, {
      info = input$behaviorsDT_cell_edit
      i = info$row
      j = info$col
      v = info$value
      pk1 <- sessionsRV()$device_id[input$sessionsDT_rows_selected]
      pk2 <- behaviorsRV()$behavior_time[i]
      pk3 <- behaviorsRV()$actor[i]
      pk4 <- behaviorsRV()$subject[i]
      colname <- names(behaviorsRV())[j]
      # cat(file=stderr(), paste0("pk1 = ", pk1, "\npk2 = ", pk2,"\npk3 = ", pk3,"\npk4 = ", pk4, "\n", "col = ", colname, "\n", "input$focalsDT_rows_selected = ", input$focalsDT_rows_selected, "\n"))
	  #cat(file=stderr(), paste0("old Cell = ", views$dat1[views$dat1$device_id==pk1 & views$dat1$behavior_time==pk2 & views$dat1$actor==pk3 & views$dat1$subject==pk4, names(views$dat1)==colname], "\n"))
       views$dat1[views$dat1$device_id==pk1 & views$dat1$behavior_time==pk2 & views$dat1$actor==pk3 & views$dat1$subject==pk4, names(views$dat1)==colname] <- v
    }) 
    
      observeEvent(input$scanListDT_cell_edit, {
      info = input$scanListDT_cell_edit
      i = info$row
      j = info$col
      v = info$value
      pk1 <- sessionsRV()$device_id[input$sessionsDT_rows_selected]
      pk2 <- scanListRV()$scan_time[i]
      colname <- names(scanListRV())[j]
      #cat(file=stderr(), paste0("pk1 = ", pk1, "\npk2 = ", pk2, "\n", "col = ", colname, "length of old scanlistval= " , sum(views$dat2$device_id==pk1 & views$dat2$scan_time==pk2), "\n"))
      #cat(file=stderr(), views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & !is.na(views$dat2$scan_time), names(views$dat2)==colname])
      views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & !is.na(views$dat2$scan_time), names(views$dat2)==colname] <- v
    }) 

      observeEvent(input$scansDT_cell_edit, {
      info = input$scansDT_cell_edit
      i = info$row
      j = info$col
      v = info$value
      pk1 <- sessionsRV()$device_id[input$sessionsDT_rows_selected]
      pk2 <- scanListRV()$scan_time[input$scanListDT_rows_selected]
      pk3 <- scansRV()$scanned_individual_id[i]
      colname <- names(scansRV())[j]
      cat(file=stderr(), paste0("pk1 = ", pk1, "\npk2 = ", pk2, "\npk3 = ", pk3, "\n", "col = ", colname, "\n", "lines affected = ", sum(na.omit(views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & views$dat2$scanned_individual_id==pk3))))
      
      views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & views$dat2$scanned_individual_id==pk3 & !is.na(views$dat2$scan_time), names(views$dat2)==colname] <- v
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



