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
      oldval <- sessionsRV()[row, col]
      pk1 <- sessionsRV()$device_ID[row]
      pk2 <- sessionsRV()$session_start_timeStamp[row]
      colname <- names(sessionsRV())[col]
      if((col%in%c(1,2) & (is.na(val) | val=="")) | 
       	   (col==1 & sum(duplicated(rbind(sessionsRV()[-row, 1:2], data.frame(device_ID=val, session_start_timeStamp=sessionsRV()[row, 2]))))>0)  | 
       	   (col==2 & sum(duplicated(rbind(sessionsRV()[-row, 1:2], data.frame(device_ID=sessionsRV()[row, 1], session_start_timeStamp=val))))>0))
       {

       	rejectEdit(session, tbl = "sessionsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", class(val), " and rolling back to value ",oldval," with row=",row," and col=",col, "\n"))
        setCellValue(session, tbl = "sessionsDT", row = row, col = col, value="toto", feedback = TRUE)
        setRowClass(session, tbl = "sessionsDT", row = row, "warning")
        setCellValue(session, tbl = "sessionsDT", row = row, col = col, value="toto", feedback = FALSE)

       	} else {	    
       tableValues$sessionsTable[tableValues$sessionsTable$device_ID==pk1 & tableValues$sessionsTable$session_start_timeStamp==pk2, colname] <- val
if(colname%in%names(tableValues$focalsTable) & sum(tableValues$focalsTable$device_ID==pk1 & tableValues$focalsTable$session_start_timeStamp==pk2)>0) tableValues$focalsTable[tableValues$focalsTable$device_ID==pk1 & tableValues$focalsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$behaviorsTable) & sum(tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$session_start_timeStamp==pk2)>0)
tableValues$behaviorsTable[tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$scansTable) & sum(tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$session_start_timeStamp==pk2)>0)
tableValues$scansTable[tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$backgroundTapsTable) & sum(tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$session_start_timeStamp==pk2)>0)
tableValues$backgroundTapsTable[tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$commentsTable) & sum(tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$session_start_timeStamp==pk2))
tableValues$commentsTable[tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$continuousVarsTable) & sum(tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$session_start_timeStamp==pk2)>0)
tableValues$continuousVarsTable[tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$session_start_timeStamp==pk2, colname] <- val       
       if(colname%in%names(tableValues$dayVarsTable) & sum(tableValues$dayVarsTable$device_ID==pk1 & tableValues$dayVarsTable$session_start_timeStamp==pk2)>0)
tableValues$dayVarsTable[tableValues$dayVarsTable$device_ID==pk1 & tableValues$dayVarsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$focalVarsTable) & sum(tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$session_start_timeStamp==pk2)>0)
tableValues$focalVarsTable[tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$scanVarsTable) & sum(tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$session_start_timeStamp==pk2)>0)
tableValues$scanVarsTable[tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$session_start_timeStamp==pk2, colname] <- val

         }  
     })
  })

##########################
##########################
##########################
  
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
      oldval <- focalsRV()[row, col]
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- focalsRV()$focal_start_timeStamp[row]
      pk3 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
if(is.null(input$sessionsDT_select)) {
	       rejectEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = "");###rejects edit if no session is selected
} else {
      colname <- names(focalsRV())[col]      
      
      
      if(sum(tableValues$focalsTable$device_ID==pk1 & tableValues$focalsTable$focal_start_timeStamp==pk2)==0 & !(is.na(val) | val==""))##add new row
         {  	
      dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
      dupRowFocal <- isolate(emptyFocalListRow())
      dupRow <- cbind(dupRowSession, dupRowFocal)
      dupRow[,colname] <- val
      tableValues$focalsTable <- smartbind(tableValues$focalsTable, dupRow)[, names(dupRowFocal)]
      	
       } else {
       if((colname=="focal_start_timeStamp" & (is.na(val) | val=="")) | 
       	   (colname=="focal_start_timeStamp" & val%in%focalsRV()$focal_start_timeStamp[-row]))##primary key "unique" violation or missing primary key value
        {
       	rejectEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
              		
       tableValues$focalsTable[tableValues$focalsTable$device_ID==pk1 & tableValues$focalsTable$focal_start_timeStamp==pk2, colname] <- val

       if(colname%in%names(tableValues$behaviorsTable)) tableValues$behaviorsTable[tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$focal_start_timeStamp==pk2, colname] <- val
       
        if(colname%in%names(tableValues$scansTable)) tableValues$scansTable[tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$focal_start_timeStamp==pk2, colname] <- val
        
        if(colname%in%names(tableValues$backgroundTapsTable)) tableValues$backgroundTapsTable[tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$focal_start_timeStamp==pk2, colname] <- val

        if(colname%in%names(tableValues$commentsTable)) tableValues$commentsTable[tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$focal_start_timeStamp==pk2, colname] <- val

        if(colname%in%names(tableValues$continuousVarsTable)) tableValues$continuousVarsTable[tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$focal_start_timeStamp==pk2, colname] <- val
        
        if(colname%in%names(tableValues$focalVarsTable)) tableValues$focalVarsTable[tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$focal_start_timeStamp==pk2, colname] <- val
        
        if(colname%in%names(tableValues$scanVarsTable)) tableValues$scanVarsTable[tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$focal_start_timeStamp==pk2, colname] <- val

     #confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
        }
        }
        }
     })
  })

############################
############################
############################

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
      oldval <- behaviorsRV()[row, col]
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- behaviorsRV()$behavior_timeStamp[row]
      pk3 <- behaviorsRV()$actor[row]
      pk4 <- behaviorsRV()$subject[row]
      #pk6 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      if(is.null(input$focalsDT_select) | is.null(input$sessionsDT_select)) {
	       rejectEdit(session, tbl = "behaviorsDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(behaviorsRV())[col]     
      
      
      if(sum(tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$behavior_timeStamp==pk2 & tableValues$behaviorsTable$actor==pk3 & tableValues$behaviorsTable$subject==pk4)==0 & !(is.na(val) | val==""))##add new row
         {
      dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
      dupRowFocal <- isolate(focalsRV()[isolate(input$focalsDT_select),])
      dupRowBehav <- isolate(behaviorsRV()[input$behaviorsDT_select,])
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowBehav)
      dupRow[,colname] <- val
      tableValues$behaviorsTable <- smartbind(tableValues$behaviorsTable, dupRow)[,names(dupRowBehav)]
       } else {
       if((col%in%c(1,2,3) & (is.na(val) | val=="")) | 
       	   (col==1 & sum(duplicated(rbind(behaviorsRV()[-row, 1:3], data.frame(behavior_timeStamp=val, behaviorsRV()[row, c(2,3)])))>0))  | 
       	   (col==2 & sum(duplicated(rbind(behaviorsRV()[-row, 1:3], data.frame(actor=val, behaviorsRV()[row, c(1,3)])))>0))   | 
       	   (col==3 & sum(duplicated(rbind(behaviorsRV()[-row, 1:3], data.frame(subject=val, behaviorsRV()[row, c(1,2)])))>0))
       ) {
       	rejectEdit(session, tbl = "behaviorsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       #confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
       tableValues$behaviorsTable[tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$behavior_timeStamp==pk2 & tableValues$behaviorsTable$actor==pk3 & tableValues$behaviorsTable$subject==pk4, colname] <- val
       }
       }
     }})
  }) 

############################
############################
############################
         
observeEvent(input$scanListDT_edit,{
    if(is.null(input$scanListDT_edit)) return(NULL);
     edit <- input$scanListDT_edit;
isolate({
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_timeStamp[row]
      #pk3 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
	  oldval <- scanListRV()[row, col]
if(is.null(input$focalsDT_select) | is.null(input$sessionsDT_select)) {
	       rejectEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(scanListRV())[col]   
      
      
      if(sum(tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$scan_timeStamp==pk2)==0 & !(is.na(val) | val==""))##add new row
         {
      dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
      dupRowFocal <- isolate(focalsRV()[isolate(input$focalsDT_select),])
      dupRowScanList <- isolate(scanListRV()[input$scanListDT_select,])##which is an empty table
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList)
      dupRow[,colname] <- val
      tableValues$scanListTable <- smartbind(tableValues$scanListTable, dupRow)[,names(dupRowScanList)]
     
       } else {
       	
       if((colname=="focal_start_timeStamp" & (is.na(val) | val=="")) | 
       	   (colname=="focal_start_timeStamp" & val%in%scanListRV()$scan_timeStamp[-row]))
 {
       	rejectEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value= oldval)
            cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
      	tableValues$scansTable[tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$scan_timeStamp==pk2, colname] <- val
      }}}    
     }
     )
  })

############################
############################
############################
    
observeEvent(input$scansDT_edit,{
    if(is.null(input$scansDT_edit)) return(NULL);
     edit <- input$scansDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      oldval <- scansRV()[row,col]
      val <- edit$val;
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_timeStamp[input$scanListDT_select]
      pk3 <- scansRV()$scanned_individual_ID[row]
      cat(file=stderr(), paste0("pk1 = ", pk1,"pk2 = ", pk2,"pk3 = ", pk3, "\n"))
 if(is.null(input$focalsDT_select) | is.null(input$sessionsDT_select) | is.null(input$scanListDT_select)) {
	       rejectEdit(session, tbl = "scansDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(scansRV())[col]
      
       if(sum(tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$scan_timeStamp==pk2 & tableValues$scansTable$scanned_individual_ID ==pk3)==0 & !(is.na(val) | val=="")){

      dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
      dupRowFocal <- isolate(focalsRV()[isolate(input$focalsDT_select),])
      dupRowScanList <- isolate(scanListRV()[input$scanListDT_select,])
      dupRowScan <- isolate(scansRV()[input$scansDT_select,])##which is an empty table
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList, dupRowScan)
      dupRow[,colname] <- val
      tableValues$scansTable <- smartbind(tableValues$scansTable, dupRow)[,names(dupRowScan)]

       } else {
       	if(col==1 & (is.na(val) | val %in% scansRV()$scanned_individual_ID[-row] | val=="" )) {
            rejectEdit(session, tbl = "scansDT", row = row, col = col, id = id, value= oldval)
            cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
      	tableValues$scansTable[tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$scan_timeStamp==pk2 & tableValues$scansTable$scanned_individual_ID ==pk3, colname] <- val
      }
     }}})
  }) 
    
