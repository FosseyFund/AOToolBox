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
       	   (col==2 & sum(duplicated(rbind(sessionsRV()[-row, 1:2], data.frame(device_ID=sessionsRV()[row, 1], session_start_timeStamp=val))))>0)
       	   )
       {
       	rejectEdit(session, tbl = "sessionsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {	    
       tableValues$sessionsTable[tableValues$sessionsTable$device_ID==pk1 & tableValues$sessionsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$focalsTable)) tableValues$focalsTable[tableValues$focalsTable$device_ID==pk1 & tableValues$focalsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$behaviorsTable))
tableValues$behaviorsTable[tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$scansTable))
tableValues$scansTable[tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$backgroundTapsTable))
tableValues$backgroundTapsTable[tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$commentsTable))
tableValues$commentsTable[tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$continuousVarsTable))
tableValues$continuousVarsTable[tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$session_start_timeStamp==pk2, colname] <- val       
       if(colname%in%names(tableValues$dayVarsTable))
tableValues$dayVarsTable[tableValues$dayVarsTable$device_ID==pk1 & tableValues$dayVarsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$focalVarsTable))
tableValues$focalVarsTable[tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$session_start_timeStamp==pk2, colname] <- val
       if(colname%in%names(tableValues$scanVarsTable))
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
	       rejectEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(focalsRV())[col]      
      
       if((is.na(pk2) | pk2=="") & nrow(focalsRV())==1 & colname=="focal_start_timeStamp" & !(is.na(val) | val=="")){##adding new focal
       	
      dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
      dupRowFocal <- isolate(emptyFocalListRow())
      dupRow <- cbind(dupRowSession, dupRowFocal)
      dupRow$focal_start_timeStamp <- val
      colnames <- names(dupRowFocal)
      tableValues$focalsTable <- smartbind(tableValues$focalsTable, dupRow)[,colnames]
      	
       } else {
       if((colname=="focal_start_timeStamp" & (is.na(val) | val=="")) | 
       	   (colname=="focal_start_timeStamp" & val%in%focalsRV()$focal_start_timeStamp[-row]))
        {
       	rejectEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
              		
       tableValues$focalsTable[tableValues$focalsTable$device_ID==pk1 & tableValues$focalsTable$focal_start_timeStamp==pk2, names(tableValues$focalsTable)==colname] <- val

       if(colname%in%names(tableValues$behaviorsTable)) tableValues$behaviorsTable[tableValues$behaviorsTable$device_ID==pk1 & tableValues$behaviorsTable$focal_start_timeStamp==pk2, names(tableValues$behaviorsTable)==colname] <- val
       
        if(colname%in%names(tableValues$scansTable)) tableValues$scansTable[tableValues$scansTable$device_ID==pk1 & tableValues$scansTable$focal_start_timeStamp==pk2, names(tableValues$scansTable)==colname] <- val
        
        if(colname%in%names(tableValues$backgroundTapsTable)) tableValues$backgroundTapsTable[tableValues$backgroundTapsTable$device_ID==pk1 & tableValues$backgroundTapsTable$focal_start_timeStamp==pk2, names(tableValues$backgroundTapsTable)==colname] <- val

        if(colname%in%names(tableValues$commentsTable)) tableValues$commentsTable[tableValues$commentsTable$device_ID==pk1 & tableValues$commentsTable$focal_start_timeStamp==pk2, names(tableValues$commentsTable)==colname] <- val

        if(colname%in%names(tableValues$continuousVarsTable)) tableValues$continuousVarsTable[tableValues$continuousVarsTable$device_ID==pk1 & tableValues$continuousVarsTable$focal_start_timeStamp==pk2, names(tableValues$continuousVarsTable)==colname] <- val
        
        if(colname%in%names(tableValues$dayVarsTable)) tableValues$dayVarsTable[tableValues$dayVarsTable$device_ID==pk1 & tableValues$dayVarsTable$focal_start_timeStamp==pk2, names(tableValues$dayVarsTable)==colname] <- val

        if(colname%in%names(tableValues$focalVarsTable)) tableValues$focalVarsTable[tableValues$focalVarsTable$device_ID==pk1 & tableValues$focalVarsTable$focal_start_timeStamp==pk2, names(tableValues$focalVarsTable)==colname] <- val
        
        if(colname%in%names(tableValues$scanVarsTable)) tableValues$scanVarsTable[tableValues$scanVarsTable$device_ID==pk1 & tableValues$scanVarsTable$focal_start_timeStamp==pk2, names(tableValues$scanVarsTable)==colname] <- val

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
      pk5 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
      pk6 <- focalsRV()$focal_start_timeStamp[input$focalsDT_select]
      if(is.null(input$focalsDT_select)) {
	       rejectEdit(session, tbl = "behaviorsDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(behaviorsRV())[col]          
        if((is.na(pk2) | is.na(pk3) | is.na(pk4)) & nrow(behaviorsRV())==1){##add new row
 
      dupRowSession <- isolate(sessionsRV()[isolate(input$sessionsDT_select),])
      dupRowFocal <- isolate(focalsRV()[isolate(input$sessionsDT_select),])
      dupRowBehav <- isolate(behaviorsRV()[input$behaviorsDT_select,])
      dupRow <- cbind(dupRowSession, dupRowFocal, dupRowBehav)
      dupRow[,colname] <- val####NEED to wait until all 3 columns are filled to write them
      colnames <- names(dupRowFocal)
      tableValues$focalsTable <- smartbind(tableValues$focalsTable, dupRow)[,colnames]

 
       	views$dat1[views$dat1$device_ID==pk1 & views$dat1$session_start_timeStamp ==pk5 & views$dat1$focal_start_timeStamp==pk6, names(views$dat1)==colname] <- val
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
       views$dat1[views$dat1$device_ID==pk1 & views$dat1$behavior_timeStamp==pk2 & views$dat1$actor==pk3 & views$dat1$subject==pk4, names(views$dat1)==colname] <- val
       }
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
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_timeStamp[row]
      pk3 <- sessionsRV()$session_start_timeStamp[input$sessionsDT_select]
	  #oldval <- scanListRV()[row, col]
if(is.null(input$focalsDT_select)) {
	       rejectEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value = "");
} else {

      colname <- names(scanListRV())[col]      
      if(is.na(pk2) & nrow(scanListRV())==1){
       	views$dat2[views$dat2$device_ID==pk1 & views$dat2$session_start_timeStamp ==pk3, names(views$dat2)==colname] <- val
       } else {
       if(col==1 & (is.na(val) | val %in% scansRV()$scanned_individual_ID[-row] | val=="" )) {
       	rejectEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value= pk2)
            cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       #cat(file=stderr(), "editing... row = ", row, " col = ", col, " val = ", val, " pk1 = ", pk1, "pk2 = ",pk2 ,"pk3= ",pk3 , "colname = ", colname,"oldval = ",oldval, "\n")
       #confirmEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value = val);
     
      #if (colname=="scan_timeStamp") pk2 <- oldval
      views$dat2[views$dat2$device_ID==pk1 & views$dat2$scan_timeStamp==pk2 & !is.na(views$dat2$scan_timeStamp), names(views$dat2)==colname] <- val
      }}}    
     }
     )
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
      oldval <- scansRV()[row,col]
      val <- edit$val;
      pk1 <- sessionsRV()$device_ID[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_timeStamp[input$scanListDT_select]
      pk3 <- scansRV()$scanned_individual_ID[row]
      cat(file=stderr(), paste0("pk1 = ", pk1,"pk2 = ", pk2,"pk3 = ", pk3, "\n"))
 if(is.null(input$scanListDT_select)) {
	       rejectEdit(session, tbl = "scansDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(scansRV())[col]
       if(is.na(pk3) & nrow(scansRV())==1){
        cat(file=stderr(), paste0("pk3 is NA, pk1 = ", pk1,"; pk2 = ", pk2,"; pk3 = ", pk3, "\n")) 
        cat(file=stderr(), paste0("scanned_individual_ID is NA. Nb rows from dat2 to edit : ", sum(views$dat2$device_ID==pk1 & views$dat2$scan_timeStamp ==pk2  & !is.na(views$dat2$scan_timeStamp)), "\n")) 
       	views$dat2[views$dat2$device_ID==pk1 & views$dat2$scan_timeStamp ==pk2 & !is.na(views$dat2$scan_timeStamp), names(views$dat2)==colname] <- val
       } else {
       	if(col==1 & (is.na(val) | val %in% scansRV()$scanned_individual_ID[-row] | val=="" )) {
            rejectEdit(session, tbl = "scansDT", row = row, col = col, id = id, value= oldval)
            cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       	cat(file=stderr(), paste0("nb rows from dat2 to edit : ", sum(views$dat2$device_ID==pk1 & views$dat2$scan_timeStamp ==pk2 & views$dat2$scanned_individual_ID==pk3   & !is.na(views$dat2$scan_timeStamp)), "\n"))
      cat(file=stderr(), paste0("pk3 = ", pk3, "\n"))
       #confirmEdit(session, tbl = "scansDT", row = row, col = col, id = id, value = val);
      views$dat2[views$dat2$device_ID==pk1 & views$dat2$scan_timeStamp==pk2 & views$dat2$scanned_individual_ID==pk3 & !is.na(views$dat2$scan_timeStamp), names(views$dat2)==colname] <- val
      }
     }}})
  }) 
    
