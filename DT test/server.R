shinyServer(function(input, output, session) {


###########################
###########################
###########################
tableValues <- reactiveValues(
						sessionsTable=NULL, 
						focalsTable=NULL, 
						behaviorsTable=NULL, 
						scansTable=NULL,
						backgroundTapsTable=NULL,
						commentsTable=NULL,
						dayVarsTable=NULL,
						focalVarsTable=NULL,
						continuousVarsTable=NULL,
						scanVarsTable=NULL
				)

tableValuesCopy <- reactiveValues(
						sessionsTable=NULL, 
						focalsTable=NULL, 
						behaviorsTable=NULL, 
						scansTable=NULL,
						backgroundTapsTable=NULL,
						commentsTable=NULL,
						dayVarsTable=NULL,
						focalVarsTable=NULL,
						continuousVarsTable=NULL,
						scanVarsTable=NULL
				)

sessionChoices <- reactiveValues(choiceList=NULL)
sessionSelected <- reactiveValues(index=1)


json.output.file.input <- reactive({
	if (is.null(input$json.output.file)) return(NULL)
	else return(readLines(input$json.output.file$datapath, warn=F))
})	

behaviors.json.input <- reactive({
    if (is.null(input$behaviors.json)) return(NULL)
    else return(readLines(input$behaviors.json$datapath, warn=F))
})

layout_info.json.input <- reactive({
    if (is.null(input$layout_info.json)) return(NULL)
    else return(readLines(input$layout_info.json$datapath, warn=F))
})

###########################
source("sessionSelect.R", local=TRUE)


###########################
###########################
###########################
source("create_empty_tables.R", local=TRUE)
source("tablesRV.R", local=TRUE)

###########################
###########################
###########################
observeEvent(input$VisualizeData, {
 #cat(file=stderr(), paste0("list of output objects: ", names(outputOptions(output)), "\n"))
if(is.null(json.output.file.input()) | is.null(behaviors.json.input()) | is.null(layout_info.json.input())) {
	tableValues$sessionsTable <- NULL
	tableValues$focalsTable <- NULL
	tableValues$behaviorsTable <- NULL
	tableValues$scansTable <- NULL
	tableValues$backgroundTapsTable <- NULL
	tableValues$commentsTable <- NULL
	tableValues$dayVarsTable <- NULL
	tableValues$focalVarsTable <- NULL
	tableValues$continuousVarsTable <- NULL
	tableValues$scanVarsTable <- NULL
	} else {
	dataOutput <- jsonOutputConversion(json.output.file.input(), behaviors.json.input(), layout_info.json.input(), colmerge=input$colmerge)
	tableValues$sessionsTable <- dataOutput$sessionsTable
	tableValues$focalsTable <- dataOutput$focalsTable
	tableValues$behaviorsTable <- dataOutput$behaviorsTable
	tableValues$scansTable <- dataOutput$scansTable
	tableValues$backgroundTapsTable <- dataOutput$backgroundTapsTable
	tableValues$commentsTable <- dataOutput$commentsTable
	tableValues$dayVarsTable <- dataOutput$dayVarsTable
	tableValues$focalVarsTable <- dataOutput$focalVarsTable
	tableValues$continuousVarsTable <- dataOutput$continuousVarsTable
	tableValues$scanVarsTable <- dataOutput$scanVarsTable

	tableValuesCopy$sessionsTable <- tableValues$sessionsTable[0,]
	tableValuesCopy$focalsTable <- tableValues$focalsTable[0,]
	tableValuesCopy$behaviorsTable <- tableValues$behaviorsTable[0,]
	tableValuesCopy$scansTable <- tableValues$scansTable[0,]
	tableValuesCopy$backgroundTapsTable <- tableValues$backgroundTapsTable[0,]
	tableValuesCopy$commentsTable <- tableValues$commentsTable[0,]
	tableValuesCopy$dayVarsTable <- tableValues$dayVarsTable[0,]
	tableValuesCopy$focalVarsTable <- tableValues$focalVarsTable[0,]
	tableValuesCopy$continuousVarsTable <- tableValues$continuousVarsTable[0,]
	tableValuesCopy$scanVarsTable <- tableValues$scanVarsTable[0,]



	if(!is.null(tableValues$sessionsTable)) {
	if(nrow(tableValues$sessionsTable)>0) {
	sessionList <- as.list(1:(nrow(tableValues$sessionsTable)+1))
	names(sessionList) <- c("ALL", paste(tableValues$sessionsTable[,1], tableValues$sessionsTable[,2], sep=' | '))
	sessionChoices$choiceList <- data.frame(tableValues$sessionsTable[,1], tableValues$sessionsTable[,2])
	updateSelectInput(session=session, inputId='sessionSelect', label = "Select a session", choices = sessionList, selected = 1)
	sessionSelected$index <- 1
	}
	}
	
output$sessionsDT <- isolate(renderD3tf({
						     cat(file=stderr(), paste0("render sessionsDTServer", "\n"))
    tableProps <- list(
      btn_reset = TRUE,
      col_types = rep("string", ncol(isolate(sessionsRV()))
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
  }))
  }

source("downloadCode.R", local=TRUE)##not sure why, but it won't be read if it's located with the other source statements...

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
    
 
##############postgres connection
DBname <- reactive({
	return(input$postgresDBname)
})

DBuser <- reactive({
	return(input$postgresUser)
})

DBhost <- reactive({
	return(input$postgresHost)
})

DBpwd <- reactive({
	return(input$postgresPwd)
})

DBport <- reactive({
	return(input$postgresPort)
})

database <- eventReactive(input$postgresConnect, {
	#cat(file=stderr(), paste(DBname(), DBhost(), DBport(), DBuser(), DBpwd(), collapse=", "))
	if(is.null(DBname()) | is.null(DBuser()) | is.null(DBhost()) | is.null(DBpwd()) | is.null(DBport())) return(NULL)
    
    drv <- dbDriver("PostgreSQL")
    	all_cons <- dbListConnections(drv)
    for(con in all_cons) dbDisconnect(con)

    con <- dbConnect(dbDriver("PostgreSQL"), dbname = tolower(DBname()), host = DBhost(), port = DBport(), user = DBuser(), password = DBpwd())
#cat(file=stderr(), paste(DBname(), DBhost(), DBport(),DBuser(), DBpwd(), collapse=", "))
    return(con)
})

connectionStatus <- reactiveValues(state=FALSE)

output$DoneConnect <- renderText({
	if(is.null(database())){
		connectionStatus$state <- FALSE
		return(NULL)
		}
		connectionStatus$state <- TRUE
		return("SUCCESS!")	
})

#############unzip and upload


observeEvent(input$runZipUpload, {
   if (is.null(input$zipFolder)) return(NULL)
   fileNames <- unzip(input$zipFolder$datapath)
   ans <- list()
   for(i in 1:length(fileNames)){
   	ans[[i]] <- read.csv(fileNames[i], header=T, check.names=F, stringsAsFactors=F)
   }
   names(ans) <- unlist(strsplit(unlist(strsplit(fileNames, split="./")), ".csv"))
   cat(file=stderr(), paste0("files extracted: ", paste(names(ans), collapse=";"),"\n"))
   if(connectionStatus$state==TRUE){
   	    cat(file=stderr(), "Uploading file...\n")
con <- database()
if(nrow(ans$sessionsTable)>0) uploadSessionsTable(ans$sessionsTable, con)
if(nrow(ans$focalsTable)>0) uploadFocalsTable(ans$focalsTable, con)
if(nrow(ans$behaviorsTable)>0) uploadBehaviorsTable(ans$behaviorsTable, con)
if(nrow(ans$scansTable)>0) uploadScansTable(ans$scansTable, con)
if(nrow(ans$scansTable)>0) uploadScanData(ans$scansTable, con)
if(nrow(ans$scansTable)>0) uploadScansIntermediateTables(ans$scansTable, con)
if(nrow(ans$behaviorsTable)>0) uploadBehaviorsIntermediateTables(ans$behaviorsTable, con)
if(nrow(ans$scanVarsTable)>0) uploadScanVariables(ans$scanVarsTable, con)
if(nrow(ans$scanVarsTable)>0) uploadscanVarsIntermediateTables(ans$scanVarsTable, con)

if("continuous_focal_variables" %in% dbListTables(con) & nrow(ans$continuousVarsTable)>0) {
uploadContinuousVariables(ans$continuousVarsTable, con)
uploadContinuousVarsIntermediateTables(ans$continuousVarsTable, con)
}
if(nrow(ans$scanVarsTable)>0) uploadFocalVariables(ans$focalVarsTable, con)
if(nrow(ans$scanVarsTable)>0) uploadfocalVarsIntermediateTables(ans$focalVarsTable, con)
if(nrow(ans$dayVarsTable)>0) uploadSessionVariables(ans$dayVarsTable, con)
if(nrow(ans$dayVarsTable)>0) uploadSessionVarsIntermediateTables(ans$dayVarsTable, con)
if(nrow(ans$backgroundTapsTable)>0) uploadBackgroundTapsTable(ans$backgroundTapsTable, con)
if(nrow(ans$commentsTable)>0) uploadCommentTable(ans$commentsTable, con)

output$DoneUploading <- renderText({
	cat(file=stderr(), "Success!\n")
		return("SUCCESS!")	
})
   }
})


##############
}
)



