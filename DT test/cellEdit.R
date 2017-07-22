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
      oldval <- behaviorsRV()[row, col]
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
        if((is.na(pk2) | is.na(pk3) | is.na(pk4)) & nrow(behaviorsRV())==1){
       	views$dat1[views$dat1$device_id==pk1 & views$dat1$session_start_time ==pk5 & views$dat1$focal_start_time==pk6, names(views$dat1)==colname] <- val
       } else {
       if((col%in%c(1,2,3) & (is.na(val) | val=="")) | 
       	   (col==1 & sum(duplicated(rbind(behaviorsRV()[-row, 1:3], data.frame(behavior_time=val, behaviorsRV()[row, c(2,3)])))>0))  | 
       	   (col==2 & sum(duplicated(rbind(behaviorsRV()[-row, 1:3], data.frame(actor=val, behaviorsRV()[row, c(1,3)])))>0))   | 
       	   (col==3 & sum(duplicated(rbind(behaviorsRV()[-row, 1:3], data.frame(subject=val, behaviorsRV()[row, c(1,2)])))>0))
       ) {
       	rejectEdit(session, tbl = "behaviorsDT", row = row, col = col, id = id, value= oldval)
        cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       #confirmEdit(session, tbl = "focalsDT", row = row, col = col, id = id, value = val);
       views$dat1[views$dat1$device_id==pk1 & views$dat1$behavior_time==pk2 & views$dat1$actor==pk3 & views$dat1$subject==pk4, names(views$dat1)==colname] <- val
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
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_time[row]
      pk3 <- sessionsRV()$session_start_time[input$sessionsDT_select]
	  #oldval <- scanListRV()[row, col]
if(is.null(input$focalsDT_select)) {
	       rejectEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value = "");
} else {

      colname <- names(scanListRV())[col]      
      if(is.na(pk2) & nrow(scanListRV())==1){
       	views$dat2[views$dat2$device_id==pk1 & views$dat2$session_start_time ==pk3, names(views$dat2)==colname] <- val
       } else {
       if(col==1 & (is.na(val) | val %in% scansRV()$scanned_individual_id[-row] | val=="" )) {
       	rejectEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value= pk2)
            cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       #cat(file=stderr(), "editing... row = ", row, " col = ", col, " val = ", val, " pk1 = ", pk1, "pk2 = ",pk2 ,"pk3= ",pk3 , "colname = ", colname,"oldval = ",oldval, "\n")
       #confirmEdit(session, tbl = "scanListDT", row = row, col = col, id = id, value = val);
     
      #if (colname=="scan_time") pk2 <- oldval
      views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & !is.na(views$dat2$scan_time), names(views$dat2)==colname] <- val
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
      pk1 <- sessionsRV()$device_id[input$sessionsDT_select]
      pk2 <- scanListRV()$scan_time[input$scanListDT_select]
      pk3 <- scansRV()$scanned_individual_id[row]
      cat(file=stderr(), paste0("pk1 = ", pk1,"pk2 = ", pk2,"pk3 = ", pk3, "\n"))
 if(is.null(input$scanListDT_select)) {
	       rejectEdit(session, tbl = "scansDT", row = row, col = col, id = id, value = "");
} else {
      colname <- names(scansRV())[col]
       if(is.na(pk3) & nrow(scansRV())==1){
        cat(file=stderr(), paste0("pk3 is NA, pk1 = ", pk1,"; pk2 = ", pk2,"; pk3 = ", pk3, "\n")) 
        cat(file=stderr(), paste0("scanned_individual_id is NA. Nb rows from dat2 to edit : ", sum(views$dat2$device_id==pk1 & views$dat2$scan_time ==pk2  & !is.na(views$dat2$scan_time)), "\n")) 
       	views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time ==pk2 & !is.na(views$dat2$scan_time), names(views$dat2)==colname] <- val
       } else {
       	if(col==1 & (is.na(val) | val %in% scansRV()$scanned_individual_id[-row] | val=="" )) {
            rejectEdit(session, tbl = "scansDT", row = row, col = col, id = id, value= oldval)
            cat(file=stderr(), paste0("Rejecting value ", val, " and rolling back to value ",oldval,"\n"))
       	} else {
       	cat(file=stderr(), paste0("nb rows from dat2 to edit : ", sum(views$dat2$device_id==pk1 & views$dat2$scan_time ==pk2 & views$dat2$scanned_individual_id==pk3   & !is.na(views$dat2$scan_time)), "\n"))
      cat(file=stderr(), paste0("pk3 = ", pk3, "\n"))
       #confirmEdit(session, tbl = "scansDT", row = row, col = col, id = id, value = val);
      views$dat2[views$dat2$device_id==pk1 & views$dat2$scan_time==pk2 & views$dat2$scanned_individual_id==pk3 & !is.na(views$dat2$scan_time), names(views$dat2)==colname] <- val
      }
     }}})
  }) 
    
