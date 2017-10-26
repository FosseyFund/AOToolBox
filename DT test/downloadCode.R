	output$downloadSessionsTable <- downloadHandler(
    filename = function() { 
		'sessionsTable.csv'
	 },
    content = function(file) {
    cat(file=stderr(), "Downloading SessionsTable.csv\n")
     write.csv(tableValues$sessionsTable, file, row.names=F, na="")
    }
  )
	output$downloadFocalsTable <- downloadHandler(
    filename = function() { 
		 'focalsTable.csv' 
	 },
    content = function(file) {
        cat(file=stderr(), "Downloading FocalsTable.csv\n")
        write.csv(tableValues$focalsTable, file, row.names=F, na="")
    }
  )
	output$downloadBehaviorsTable <- downloadHandler(
    filename = function() { 
		 'behaviorsTable.csv'
	 },
    content = function(file) {
     write.csv(tableValues$behaviorsTable, file, row.names=F, na="")
    }
  )
	output$downloadScansTable <- downloadHandler(
    filename = function() { 
		'scansTable.csv'
	 },
    content = function(file) {
     write.csv(tableValues$scansTable, file, row.names=F, na="")
    }
  )
  	output$downloadBackgroundTapsTable <- downloadHandler(
    filename = function() { 
		'backgroundTapsTable.csv'
	 },
    content = function(file) {
     write.csv(tableValues$backgroundTapsTable, file, row.names=F, na="")
    }
  )
   	output$downloadCommentsTable <- downloadHandler(
    filename = function() { 
		 'commentsTable.csv'
	 },
    content = function(file) {
     write.csv(tableValues$commentsTable, file, row.names=F, na="")
    }
  )
  output$downloadDayVarsTable <- downloadHandler(
    filename = function() { 
		 'dayVarsTable.csv' 
	 },
    content = function(file) {
     write.csv(tableValues$dayVarsTable, file, row.names=F, na="")
    }
  )
  output$downloadFocalVarsTable <- downloadHandler(
    filename = function() { 
		 'focalVarsTable.csv'
	 },
    content = function(file) {
     write.csv(tableValues$focalVarsTable, file, row.names=F, na="")
    }
  )
  output$downloadContinuousVarsTable <- downloadHandler(
    filename = function() { 
		 'continuousVarsTable.csv'
	 },
    content = function(file) {
     write.csv(tableValues$continuousVarsTable, file, row.names=F, na="")
    }
  )
  output$downloadScanVarsTable <- downloadHandler(
    filename = function() { 
		 'scanVarsTable.csv' 
	 },
    content = function(file) {
     write.csv(tableValues$scanVarsTable, file, row.names=F, na="")
    }
  )
  
  output$downloadZip <- downloadHandler(
       filename = function() {
         paste("AO_OutPut_", Sys.time(), ".zip", sep="")
       },
       content = function(fname) {
         fs <- c()
         tmpdir <- tempdir()
         initwd <- getwd()
         setwd(tempdir())

         	write.csv(tableValues$sessionsTable, file="sessionsTable.csv", row.names=F, na="")
         	write.csv(tableValues$focalsTable, file="focalsTable.csv", row.names=F, na="")
         	write.csv(tableValues$behaviorsTable, file="behaviorsTable.csv", row.names=F, na="")
         	write.csv(tableValues$scansTable, file="scansTable.csv", row.names=F, na="")
         	write.csv(tableValues$backgroundTapsTable, file="backgroundTapsTable.csv", row.names=F, na="")
         	write.csv(tableValues$commentsTable, file="commentsTable.csv", row.names=F, na="")
         	write.csv(tableValues$dayVarsTable, file="dayVarsTable.csv", row.names=F, na="")
         	write.csv(tableValues$focalVarsTable, file="focalVarsTable.csv", row.names=F, na="")
         	write.csv(tableValues$continuousVarsTable, file="continuousVarsTable.csv", row.names=F, na="")
         	write.csv(tableValues$scanVarsTable, file="scanVarsTable.csv", row.names=F, na="")
            zip(zipfile=fname, files=paste0(names(tableValues), ".csv"))
       },
       contentType = "application/zip"
     )
