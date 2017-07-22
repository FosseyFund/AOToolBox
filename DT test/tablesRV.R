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
