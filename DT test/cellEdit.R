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
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval," with row=",row," and col=",col, "\n"))
        #setCellValue(session, tbl = "sessionsDT", row = row, col = col, value="toto", feedback = TRUE)
        #setRowClass(session, tbl = "sessionsDT", row = row, "warning")
        #setCellValue(session, tbl = "sessionsDT", row = row, col = col, value="toto", feedback = FALSE)
return(NULL)
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


     if(col%in%c(1,2)){
     if(!is.null(tableValues$sessionsTable)) {
	if(nrow(tableValues$sessionsTable)>0) {
	sessionList <- as.list(1:(nrow(tableValues$sessionsTable)+1))
	names(sessionList) <- c("ALL", paste(tableValues$sessionsTable[,1], tableValues$sessionsTable[,2], sep=' | '))
	sessionChoices$choiceList <- data.frame(tableValues$sessionsTable[,1], tableValues$sessionsTable[,2])
	updateSelectInput(session=session, inputId='sessionSelect', label = "Select a session", choices = sessionList, selected = sessionSelected$index)
	}
	}
    }
     
     
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
      tableValues$focalsTable <- smartbind(tableValues$focalsTable, dupRow)[, names(tableValues$focalsTable)]
      	
       } else {
       if((colname=="focal_start_timeStamp" & (is.na(val) | val=="")) | 
       	   (colname=="focal_start_timeStamp" & val%in%focalsRV()$focal_start_timeStamp[-row]))##primary key "unique" violation or missing primary key value
        {
       	rejectEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
              		
       tableValues$focalsTable[tableValues$focalsTable$device_ID==pk1 & tableValues$focalsTable$focal_start_timeStamp==pk2, colname] <- val

       if(colname%in%names(tableValues$behaviorsTable) & sum(tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$focal_start_timeStamp==pk2)>0) tableValues$behaviorsTable[tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$focal_start_timeStamp==pk2, colname] <- val
       
        if(colname%in%names(tableValues$scansTable) & sum(tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$focal_start_timeStamp==pk2)>0) tableValues$scansTable[tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$focal_start_timeStamp==pk2, colname] <- val
        
        if(colname%in%names(tableValues$backgroundTapsTable) & sum(tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$focal_start_timeStamp==pk2)>0) tableValues$backgroundTapsTable[tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$focal_start_timeStamp==pk2, colname] <- val

        if(colname%in%names(tableValues$commentsTable) & sum(tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$focal_start_timeStamp==pk2)>0) tableValues$commentsTable[tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$focal_start_timeStamp==pk2, colname] <- val

        if(colname%in%names(tableValues$continuousVarsTable) & sum(tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$focal_start_timeStamp==pk2)>0) tableValues$continuousVarsTable[tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$focal_start_timeStamp==pk2, colname] <- val
        
        if(colname%in%names(tableValues$focalVarsTable) & sum(tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$focal_start_timeStamp==pk2)>0) tableValues$focalVarsTable[tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$focal_start_timeStamp==pk2, colname] <- val
        
        if(colname%in%names(tableValues$scanVarsTable) & sum(tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$focal_start_timeStamp==pk2)>0) tableValues$scanVarsTable[tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$focal_start_timeStamp==pk2, colname] <- val

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
 
      if(is.null(input$focalsDT_select) | is.null(input$sessionsDT_select) | sum(focalsRV()$focal_individual_ID[input$focalsDT_select]=="")==1 | sum(focalsRV()$focal_start_timeStamp[input$focalsDT_select]=="")==1)  {
           cat(file=stderr(),"rejecting\n")
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
      tableValues$behaviorsTable <- smartbind(tableValues$behaviorsTable, dupRow)[,names(tableValues$behaviorsTable)]
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

observeEvent(input$dayVarsDT_edit,{
    if(is.null(input$dayVarsDT_edit)) return(NULL);
     edit <- input$dayVarsDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      oldval <- dayVarsRV()[row, col]
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- dayVarsRV()$dayVars[input$dayVarsDT_select]
      
      if(is.null(input$dayVarsDT_select) | is.null(input$sessionsDT_select)) {
	       rejectEdit(session, tbl = "dayVarsDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(dayVarsRV())[col]     
      
      
      if(sum(tableValues$dayVarsTable$device_ID==pk1 & tableValues$dayVarsTable$session_start_timeStamp==pk2 & tableValues$dayVarsTable$dayVars==pk3)==0 & !(is.na(val) | val==""))##add new row
         {
      cat(file=stderr(), paste0("adding row!\n"))
	
      dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
      dupRowDayVar <- isolate(dayVarsRV()[input$dayVarsDT_select,])
      dupRow <- cbind(dupRowSession, dupRowDayVar)
      dupRow[,colname] <- val
      tableValues$dayVarsTable <- smartbind(tableValues$dayVarsTable, dupRow)[,names(tableValues$dayVarsTable)]
       } else {
       	
       	
 	   if(col==1 & (is.na(val) | val%in%dayVarsRV()$dayVars[-row] | val=="" )) {
       	rejectEdit(session, tbl = "dayVarsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       #confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
       cat(file=stderr(), paste0("editing!\n"))
       tableValues$dayVarsTable[(tableValues$dayVarsTable$device_ID==pk1 & tableValues$dayVarsTable$session_start_timeStamp==pk2 & tableValues$dayVarsTable$dayVars==pk3), colname] <- val
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
if(is.null(input$focalsDT_select) | is.null(input$sessionsDT_select)  | sum(focalsRV()$focal_individual_ID[input$focalsDT_select]=="")==1 | sum(focalsRV()$focal_start_timeStamp[input$focalsDT_select]=="")==1) {
	       cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to ''","\n"))
	       rejectEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(scanListRV())[col]
      
      if(sum(tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$scan_timeStamp==pk2)==0 & !(is.na(val) | val==""))##add new row
         {
       cat(file=stderr(), paste0("Should add new scanList row\n"))

      dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
      dupRowFocal <- isolate(focalsRV()[isolate(input$focalsDT_select),])
      dupRowScanList <- isolate(scanListRV()[input$scanListDT_select,])##which is an empty table
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList)
      dupRow[,colname] <- val
      
      tableValues$scansTable <- smartbind(tableValues$scansTable, dupRow)[,names(tableValues$scansTable)]
           tableValues$scansTable[is.na(tableValues$scansTable)] <- ""

       } else {
       	
       if((colname=="scan_timeStamp" & (is.na(val) | val=="")) | 
       	   (colname=="scan_timeStamp" & val%in%scanListRV()$scan_timeStamp[-row]))
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
      pk3 <- scansRV()$scanned_individual_ID[row]## is "" if no scan data
      cat(file=stderr(), paste0("pk1 = ", pk1,"pk2 = ", pk2,"pk3 = ", pk3, "\n"))
 if(is.null(input$focalsDT_select) | is.null(input$sessionsDT_select) | is.null(input$scanListDT_select) | sum(scanListRV()$scan_timeStamp[input$scanListDT_select]=="")==1) { ##case where no parent table is selected
	  rejectEdit(session, tbl = "scansDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(scansRV())[col]
      #cat(file=stderr(), paste0("tag1 with sum(tableValues$scansTable$device_ID==pk1)=", sum(tableValues$scansTable$device_ID==pk1)," \n"))
      #cat(file=stderr(), paste0("tag11 with sum(tableValues$scansTable$scan_timeStamp==pk2)=", sum(tableValues$scansTable$scan_timeStamp==pk2)," \n"))
      #cat(file=stderr(), paste0("tag12 with sum(tableValues$scansTable$scanned_individual_ID ==pk3)=", sum(tableValues$scansTable$scanned_individual_ID ==pk3)," \n"))

       if(sum(tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$scan_timeStamp==pk2 & tableValues$scansTable$scanned_individual_ID ==pk3)==0 & !(is.na(val) | val=="")){

      dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
      dupRowFocal <- isolate(focalsRV()[isolate(input$focalsDT_select),])
      dupRowScanList <- isolate(scanListRV()[input$scanListDT_select,])
      dupRowScan <- isolate(scansRV()[input$scansDT_select,])##which is an empty table
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList, dupRowScan)
      dupRow[,colname] <- val
      tableValues$scansTable <- smartbind(tableValues$scansTable, dupRow)[,tableValues$scansTable]

       } else {
       	#cat(file=stderr(), paste0("tag2\n"))
       	if(col==1 & (is.na(val) | val %in% scansRV()$scanned_individual_ID[-row] | val=="" )) {
                  #cat(file=stderr(), paste0("tag3\n"))
            rejectEdit(session, tbl = "scansDT", row = row, col = col, id = id, value= oldval)
            cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       		                  cat(file=stderr(), paste0("tag4\n"))

       	#shouldBeOne <- (tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$scan_timeStamp==pk2 & tableValues$scansTable$scanned_individual_ID ==pk3)
       	#cat(file=stderr(), paste0("Replacing x values with x=", shouldBeOne, "\n"))
      	tableValues$scansTable[tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$scan_timeStamp==pk2 & tableValues$scansTable$scanned_individual_ID ==pk3, colname] <- val
      	cat(file=stderr(), paste0("Successfully replaced value\n"))
      }
     }}})
  }) 
    
############################
############################
############################

observeEvent(input$commentsDT_edit,{
    if(is.null(input$commentsDT_edit)) return(NULL);
     edit <- input$commentsDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      cat(file=stderr(), paste0("Val = ", val,"\n"))
      oldval <- commentsRV()[row, col]
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk4 <- commentsRV()$comment_timeStamp[input$commentsDT_select]
     
      if(is.null(input$commentsDT_select) | is.null(input$sessionsDT_select) | is.null(input$focalsDT_select) | sum(focalsRV()$focal_individual_ID[input$focalsDT_select]=="")==1 | sum(focalsRV()$focal_start_timeStamp[input$focalsDT_select]=="")==1) {
	       rejectEdit(session, tbl = "commentsDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(commentsRV())[col]     
      
      
      if(sum(tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$session_start_timeStamp==pk2 & tableValues$commentsTable$focal_start_timeStamp ==pk3  & tableValues$commentsTable$comment_timeStamp ==pk4)==0 & !(is.na(val) | val==""))##add new row
         {
      cat(file=stderr(), paste0("adding row!\n"))
	  
	  dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
	  dupRowFocal <- isolate(focalsRV()[isolate(input$focalsDT_select),])
      dupRowComment <- isolate(commentsRV()[input$commentsDT_select,])
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowComment)
      dupRow[,colname] <- val
      tableValues$commentsTable <- smartbind(tableValues$commentsTable, dupRow)[,names(tableValues$commentsTable)]
       } else {
       	
 	   if(col==1 & (is.na(val) | val%in%commentsRV()$comment_timeStamp[-row] | val=="" )) {
       	rejectEdit(session, tbl = "commentsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       #confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
       cat(file=stderr(), paste0("editing!\n"))
       tableValues$commentsTable[(tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$session_start_timeStamp==pk2 & tableValues$commentsTable$focal_start_timeStamp ==pk3 & tableValues$commentsTable$comment_timeStamp ==pk4), colname] <- val
       }
       }
     }})
 })
 
############################
############################
############################

observeEvent(input$backgroundTapsDT_edit,{
    if(is.null(input$backgroundTapsDT_edit)) return(NULL);
     edit <- input$backgroundTapsDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      oldval <- backgroundTapsRV()[row, col]
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk4 <- backgroundTapsRV()$backgroundTap_timeStamp[input$backgroundTapsDT_select]
     
      if(is.null(input$backgroundTapsDT_select) | is.null(input$sessionsDT_select) | is.null(input$focalsDT_select) | sum(focalsRV()$focal_individual_ID[input$focalsDT_select]=="")==1 | sum(focalsRV()$focal_start_timeStamp[input$focalsDT_select]=="")==1) {
	       rejectEdit(session, tbl = "backgroundTapsDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(backgroundTapsRV())[col]     
      
      
      if(sum(tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$session_start_timeStamp==pk2 & tableValues$backgroundTapsTable$focal_start_timeStamp ==pk3  & tableValues$backgroundTapsTable$backgroundTap_timeStamp ==pk4)==0 & !(is.na(val) | val==""))##add new row
         {
      cat(file=stderr(), paste0("adding row!\n"))
	  
	  dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
	  dupRowFocal <- isolate(focalsRV()[isolate(input$focalsDT_select),])
      dupRowBackgroundTap <- isolate(backgroundTapsRV()[input$backgroundTapsDT_select,])
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowBackgroundTap)
      dupRow[,colname] <- val
      tableValues$backgroundTapsTable <- smartbind(tableValues$backgroundTapsTable, dupRow)[,names(tableValues$backgroundTapsTable)]
       } else {
       	
 	   if(col==1 & (is.na(val) | val%in%backgroundTapsRV()$backgroundTap_timeStamp[-row] | val=="" )) {
       	rejectEdit(session, tbl = "backgroundTapsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       #confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
       cat(file=stderr(), paste0("editing!\n"))
       tableValues$backgroundTapsTable[(tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$session_start_timeStamp==pk2 & tableValues$backgroundTapsTable$focal_start_timeStamp ==pk3 & tableValues$backgroundTapsTable$backgroundTap_timeStamp ==pk4), colname] <- val
       }
       }
     }})
 })
 
 
############################
############################
############################

observeEvent(input$focalVarsDT_edit,{
    if(is.null(input$focalVarsDT_edit)) return(NULL);
     edit <- input$focalVarsDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      oldval <- focalVarsRV()[row, col]
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk4 <- focalVarsRV()$focalVars[input$focalVarsDT_select]
     
      if(is.null(input$focalVarsDT_select) | is.null(input$sessionsDT_select) | is.null(input$focalsDT_select) | sum(focalsRV()$focal_individual_ID[input$focalsDT_select]=="")==1 | sum(focalsRV()$focal_start_timeStamp[input$focalsDT_select]=="")==1) {
	       rejectEdit(session, tbl = "focalVarsDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(focalVarsRV())[col]     
      
      
      if(sum(tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$session_start_timeStamp==pk2 & tableValues$focalVarsTable$focal_start_timeStamp ==pk3  & tableValues$focalVarsTable$focalVars==pk4)==0 & !(is.na(val) | val==""))##add new row
         {
      cat(file=stderr(), paste0("adding row!\n"))
	  
	  dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
	  dupRowFocal <- isolate(focalsRV()[isolate(input$focalsDT_select),])
      dupRowFocalVars <- isolate(focalVarsRV()[input$focalVarsDT_select,])
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowFocalVars)
      dupRow[,colname] <- val
      tableValues$focalVarsTable <- smartbind(tableValues$focalVarsTable, dupRow)[,names(tableValues$focalVarsTable)]
       } else {
       	
 	   if(col==1 & (is.na(val) | val%in%focalVarsRV()$focalVars[-row] | val=="" )) {
       	rejectEdit(session, tbl = "focalVarsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       #confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
       cat(file=stderr(), paste0("editing!\n"))
       tableValues$focalVarsTable[(tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$session_start_timeStamp==pk2 & tableValues$focalVarsTable$focal_start_timeStamp ==pk3 & tableValues$focalVarsTable$focalVars ==pk4), colname] <- val
       }
       }
     }})
 })
 
############################
############################
############################

observeEvent(input$continuousVarsDT_edit,{
    if(is.null(input$continuousVarsDT_edit)) return(NULL);
     edit <- input$continuousVarsDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      oldval <- continuousVarsRV()[row, col]
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk4 <- continuousVarsRV()$continuousVars[input$continuousVarsDT_select]
     
      if(is.null(input$continuousVarsDT_select) | is.null(input$sessionsDT_select) | is.null(input$focalsDT_select) | sum(focalsRV()$focal_individual_ID[input$focalsDT_select]=="")==1 | sum(focalsRV()$focal_start_timeStamp[input$focalsDT_select]=="")==1) {
	       rejectEdit(session, tbl = "continuousVarsDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(continuousVarsRV())[col]     
      
      
      if(sum(tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$session_start_timeStamp==pk2 & tableValues$continuousVarsTable$focal_start_timeStamp ==pk3  & tableValues$continuousVarsTable$continuousVars==pk4)==0 & !(is.na(val) | val==""))##add new row
         {
      cat(file=stderr(), paste0("adding row!\n"))
	  
	  dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
	  dupRowFocal <- isolate(focalsRV()[isolate(input$focalsDT_select),])
      dupRowContinuousVars <- isolate(continuousVarsRV()[input$continuousVarsDT_select,])
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowContinuousVars)
      dupRow[,colname] <- val
      tableValues$continuousVarsTable <- smartbind(tableValues$continuousVarsTable, dupRow)[,names(tableValues$continuousVarsTable)]
       } else {
       	
 	   if(col==1 & (is.na(val) | val%in% continuousVarsRV()$continuousVars[-row] | val=="" )) {
       	rejectEdit(session, tbl = "continuousVarsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {

       cat(file=stderr(), paste0("editing!\n"))
       tableValues$continuousVarsTable[(tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$session_start_timeStamp==pk2 & tableValues$continuousVarsTable$focal_start_timeStamp ==pk3 & tableValues$continuousVarsTable$continuousVars ==pk4), colname] <- val
       }
       }
     }})
 })
 
 
############################
############################
############################

observeEvent(input$scanVarsDT_edit,{
    if(is.null(input$scanVarsDT_edit)) return(NULL);
     edit <- input$scanVarsDT_edit;
isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      oldval <- scanVarsRV()[row, col]
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk3 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      pk4 <- scanListRV()$scan_timeStamp[input$scanListDT_select]
      pk5 <- scanVarsRV()$scanVars[input$scanVarsDT_select]
     
      if(is.null(input$scanVarsDT_select) | is.null(input$sessionsDT_select) | is.null(input$focalsDT_select) | is.null(input$scanListDT_select) | sum(scanListRV()$scan_timeStamp[input$scanListDT_select]=="")==1) {
	       rejectEdit(session, tbl = "scanVarsDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(scanVarsRV())[col]     
      
      
      if(sum(tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$session_start_timeStamp==pk2 & tableValues$scanVarsTable$focal_start_timeStamp ==pk3  & tableValues$scanVarsTable$scan_timeStamp==pk4 & tableValues$scanVarsTable$scanVars==pk5)==0 & !(is.na(val) | val==""))##add new row
         {
      cat(file=stderr(), paste0("adding row!\n"))
	  
	  dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
	  dupRowFocal <- isolate(focalsRV()[isolate(input$focalsDT_select),])
	  dupRowScanList <- isolate(scanListRV()[isolate(input$scanListDT_select),])
	  dupRowScanVars <- isolate(scanVarsRV()[input$scanVarsDT_select,])
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowScanList, dupRowScanVars)
      dupRow[,colname] <- val
      tableValues$scanVarsTable <- smartbind(tableValues$scanVarsTable, dupRow)[,names(tableValues$scanVarsTable)]
       } else {
       	
 	   if(col==1 & (is.na(val) | val%in%scanVarsRV()$scanVars[-row] | val=="" )) {
       	rejectEdit(session, tbl = "scanVarsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {

       cat(file=stderr(), paste0("editing!\n"))
       tableValues$scanVarsTable[(tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$session_start_timeStamp==pk2 & tableValues$scanVarsTable$focal_start_timeStamp ==pk3 & tableValues$scanVarsTable$scan_timeStamp==pk4 & tableValues$scanVarsTable$scanVars ==pk5), colname] <- val
       }
       }
     }})
 })