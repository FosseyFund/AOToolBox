shinyServer(function(input, output, session) {
test_existance <- function(x){
	if(exists(x)) return(get(x)) else return(NULL)}
	
views <- reactiveValues(dat1= test_existance("dat1"), dat2= test_existance("dat2"))

##function to remove duplicated values and select columns
removeDuplicates <- function(dat, vec){
    	temp <- dat[,match(vec, names(dat))]
    	return(temp[!duplicated(temp),])
}


###########################
###########################
###########################

json.output.file.input <- reactive({
	if (is.null(input$json.output.file)){
		cat(file=stderr(), paste0("no json.output.file yet\n"))
		return(NULL)
	} else {
		readLines(input$json.output.file$datapath, warn=F)
    cat(file=stderr(), paste0("loading json.output.file\n"))
	}
})	


# json.output.file.input <- reactive({
	# cat(file=stderr(), paste0("about to load json.output.file\n"))
   # if (is.null(input$json.output.file))
      # return(NULL)
    # readLines(input$json.output.file$datapath, warn=F)
    # cat(file=stderr(), paste0("loading json.output.file\n"))
  # })

behaviors.json.input <- reactive({
    if (is.null(input$behaviors.json))
      return(NULL)
    readLines(input$behaviors.json$datapath, warn=F)
    cat(file=stderr(), paste0("loading behaviors.json.output\n"))
  })

layout_info.json.input <- reactive({
    if (is.null(input$layout_info.json))
      return(NULL)
    readLines(input$layout_info.json$datapath, warn=F)
    cat(file=stderr(), paste0("loading layout_info.json.output\n"))
  })

dataOutput <- reactive({
		if(is.null(json.output.file.input()) | is.null(behaviors.json.input()) | is.null(layout_info.json.input())) {return(NULL)} else {
		jsonOutputConversion(json.output.file.input(), behaviors.json.input(), layout_info.json.input(), colmerge=input$colmerge)
		cat(file=stderr(), paste0("dataOutput()$sessionsTable : ", names(dataOutput()$sessionsTable),"\n"))
		}
})



###########################
###########################
###########################
source("create_empty_tables.R", local=TRUE)
source("tablesRV.R", local=TRUE)

###########################
###########################
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



