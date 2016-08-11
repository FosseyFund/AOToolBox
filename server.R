# Define server logic for random distribution application
shinyServer(function(input, output, session) {

###########################################
################1st tab
dyadicInput <- reactive({
   	dyadic <- input$dyadic
    if (is.null(dyadic))
      return(NULL)
    read.csv(dyadic$datapath)
  })
scanInput <- reactive({
   	scan <- input$scan
    if (is.null(scan))
      return(NULL)
    read.csv(scan$datapath)
  })
soloInput <- reactive({
   	solo <- input$solo
    if (is.null(solo))
      return(NULL)
    read.csv(solo$datapath)
  })	
foodInput <- reactive({
   	food <- input$foods
    if (is.null(food))
      return(NULL)
    read.csv(food$datapath)
  })
  
textInput <- reactive({
	version <- input$version
	return(version)
})

observeEvent(input$link_to_structure, {
  newvalue <- "Create behavioral protocol file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_studyanimals, {
  newvalue <- "Create group composition file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_globalvar, {
  newvalue <- "Create global variables file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_customization, {
  newvalue <- "Additional customizations"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_foods, {
  newvalue <- "Foods"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_dyad, {
  newvalue <- "Dyadic"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_scan, {
  newvalue <- "Scan"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_solo, {
  newvalue <- "Solo"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_animals, {
  newvalue <- "Create group composition file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_welcome, {
  newvalue <- "What is Animal Observer?"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_welcome2, {
  newvalue <- "What is Animal Observer?"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_grp, {
  newvalue <- "Group composition"
  updateTabsetPanel(session,"panels3",newvalue)
})

observeEvent(input$link_to_globalvar2, {
  newvalue <- "Create global variables file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_globalvar3, {
  newvalue <- "Create global variables file"
  updateTabsetPanel(session,"panels",newvalue)
})
  
dataOutput1 <- eventReactive(input$run, {
    if(is.null(dyadicInput()) | is.null(scanInput()) | is.null(soloInput()) | textInput()=="vX.X"){
			return(NULL)
			} else 	if (is.null(foodInput())) {
			return(prepareBehaviorsJson(dyadicInput(), scanInput(), soloInput(),textInput()))
			} else {
			allBehaviorsTables <- dyadicScanSolo(dyadicInput(), scanInput(), soloInput(), foodInput())
			return(prepareBehaviorsJson(allBehaviorsTables$dyadic.all, allBehaviorsTables$scan.all, allBehaviorsTables$solo.all, textInput()))
			}
})

output$text1 <- renderText({
	if(is.null(dataOutput1())){return(NULL)}
		return("DONE!")	
		})


output$downloadBehaviorsJson <- downloadHandler(
    filename = function() { 
		 paste('behaviors.json') 
	 },
    content = function(file) {
    	writeLines(dataOutput1(), con=file)
    }
  )

###########################################
################2nd tab
compoInput <- reactive({
   	compo <- input$compo
    if (is.null(compo))
      return(NULL) else {
    return(prepareGroupCompo(read.csv(compo$datapath)))
    }
})

output$text2 <- renderText({
	if(is.null(compoInput())) return(NULL) else return("DONE")
})

output$downloadAnimalsJson <- downloadHandler(
    filename = function() {
		 paste('animals.json') 
	 },
    content = function(file) {
    	writeLines(compoInput(), con=file)
    }
)

###########################################
################3rd tab

values <-  reactiveValues()

dataOutput2 <- eventReactive(input$template, {
  if(input$run3=="upl" & is.null(dataOutput3())){
    return(NULL)
  } else if (input$run3=="upl" & !is.null(dataOutput3())){
  	#cat(file=stderr(), "input$run3==upl")
  	#cat(file=stderr(), paste(dataOutput3()))
    return(dataOutput3())
  } else {
  	#cat(file=stderr(), "input$run3==template")
return(readLayoutJson(fromJSON(file="layout_info_default.json")))
  }
})

dataOutput3 <- reactive({
  if(is.null(input$layout)){
    return(NULL)
  } else {
    return(readLayoutJson(fromJSON(paste(readLines(input$layout$datapath, warn=F), collapse=""))))
  }
})


dataPinLayout = reactive({
    if(is.null(dataOutput2())) return(NULL)
    if(is.null(input$layoutPin)){
    	temp = dataOutput2()
    	MAT <- temp[[1]]
    } else {
    	MAT=hot_to_r(input$layoutPin)
    }
     values[["pinLayout"]] = MAT
     return(MAT)
  })

dataOptionsLayout = reactive({  
    if(is.null(dataOutput2())) return(NULL)
    if(is.null(input$layoutOptions)){
    	temp = dataOutput2()
    	MAT <- as.matrix(data.frame(settings=names(temp[[2]]), values=unlist(temp[[2]]), stringsAsFactors=F))[-1,]
    } else {
    	MAT=hot_to_r(input$layoutOptions)
    }
     values[["optionsLayout"]] = MAT
     return(MAT)
  })
  
dayVarsInput <- reactive({
   	dayVars <- input$dayVars
    if (is.null(dayVars))
      return(NULL)
    read.csv(dayVars$datapath)
  })
focalVarsInput <- reactive({
   	focalVars <- input$focalVars
    if (is.null(focalVars))
      return(NULL)
    read.csv(focalVars$datapath)
  })
scanVarsInput <- reactive({
   	scanVars <- input$scanVars
    if (is.null(scanVars))
      return(NULL)
    read.csv(scanVars$datapath)
  })	  
contVarsInput <- reactive({
   	contVars <- input$contVars
    if (is.null(contVars))
      return(NULL)
    read.csv(contVars$datapath)
  })


dataOutput4 <- eventReactive(input$run2, {
	#cat(file=stderr(), "inside dataOutput4\n")

    if(is.null(dayVarsInput()) | is.null(focalVarsInput()) | is.null(scanVarsInput()) | versionTextInput()=="vX.X"){
    		#cat(file=stderr(), "dataOutput4 returning NULL\n")

			return(NULL)
			}
			
    	temp <- list()
    	temp[[1]] <- values[["pinLayout"]]  
    	temp[[2]] <- listFromCsv(dat = dayVarsInput())
    	temp[[3]] <- listFromCsv(dat = focalVarsInput())
   		temp[[4]] <- listFromCsv(dat = scanVarsInput())
   		temp[[5]] <- listFromCsv(dat = contVarsInput())
	
	optionnames <- values[["optionsLayout"]][,1]
	optionvalues<- as.character(values[["optionsLayout"]][,2])
	
	temp[[6]] <- data.frame(values=c(versionTextInput(), optionvalues))
	temp[[6]] <- as.data.frame(t(as.matrix(temp[[6]])))
	names(temp[[6]]) <- c("version", optionnames)
	#cat(file=stderr(), paste(temp))
    return(createLayoutJSON(temp))
})

output$text3 <- renderText({
	if(is.null(dataOutput4())){return(NULL)}
		return("DONE!")	
		})


output$downloadLayoutJson <- downloadHandler(
    filename = function() { 
		 paste('layout_info.json') 
	 },
    content = function(file) {
    	writeLines(dataOutput4(), con=file)
    }
  )

output$layoutOptions <- renderRHandsontable({
    MAT = dataOptionsLayout()
       if (!is.null(MAT)) {
        return(rhandsontable(MAT, usesTypes=F, rowHeaders=1:nrow(MAT)) %>%
      hot_table(highlightCol = FALSE, highlightRow = TRUE,
            allowRowEdit = FALSE,
            columnSorting = FALSE,exportToCsv = TRUE) %>%
            hot_col(col="settings", readOnly=TRUE)
    )
    }
  })

output$layoutPin <- renderRHandsontable({
    MAT = dataPinLayout()
       if (!is.null(MAT)) {
      return(rhandsontable(MAT, useTypes=F, rowHeaders=1:nrow(MAT)) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE,
            columnSorting = FALSE, exportToCsv = TRUE) %>%
            hot_context_menu(allowRowEdit=TRUE, allowColEdit=FALSE)
       )
    }
  })

versionTextInput <- reactive({
	version <- input$versionLayout
	return(version)
})
  

output$TextPin <- renderText({
	if (!(is.null(dataOutput2()) & is.null(dataOutput3()))){
		"Enter the pin codes corresponding to the different users. Right-click to #add/delete rows."
	}
})

output$TextOptions <- renderText({
	if (!is.null(dataOutput2())){
		"Options: fill in the second column of the following table, making sure you respect data types (eg, booleans must be TRUE or FALSE, numbers must not be characters)."
	}
})


###########################################
################4th tab
json.output.file.input <- reactive({
   if (is.null(input$json.output.file))
      return(NULL)
    readLines(input$json.output.file$datapath, warn=F)
  })
  
behaviors.json.input <- reactive({
    if (is.null(input$behaviors.json))
      return(NULL)
    readLines(input$behaviors.json$datapath, warn=F)
  })
layout_info.json.input <- reactive({
    if (is.null(input$layout_info.json))
      return(NULL)
    readLines(input$layout_info.json$datapath, warn=F)
  })

dataOutput <- reactive({
		if(is.null(json.output.file.input()) | is.null(behaviors.json.input()) | is.null(layout_info.json.input())) {return(NULL)} else 
		jsonOutputConversion(json.output.file.input(), behaviors.json.input(), layout_info.json.input())
		})
		
	output$sessionsTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"sessionsTable.csv"
		})
	output$focalsTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"focalsTable.csv"
		})
	output$behaviorsTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"behaviorsTable.csv"
		})
	output$scansTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"scansTable.csv"
		})
	output$backgroundTapsTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"backgroundTapsTable.csv"
		})
	output$commentsTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"commentsTable.csv"
		})
    output$dayVarsTable.csv <- renderText({
    	if(is.null(dataOutput())) return(NULL)
    		"dayVarsTable.csv"
    		})
    	output$focalVarsTable.csv <- renderText({
    if(is.null(dataOutput())) return(NULL)
   		"focalVarsTable.csv"
    	})
    output$continuousVarsTable.csv <- renderText({
   	if(is.null(dataOutput())) return(NULL)
   		"continuousVarsTable.csv"
    	})
    output$scanVarsTable.csv <- renderText({
   	if(is.null(dataOutput())) return(NULL)
   		"scanVarsTable.csv"
   		})
    		

		
	output$table1 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$sessionsTable
		}, include.rownames=F)
	
	output$table2 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$focalsTable
		}, include.rownames=F)

	output$table3 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$behaviorsTable
		}, include.rownames=F)

	output$table4 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$scansTable
		}, include.rownames=F)
		
	output$table5 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$backgroundTapsTable
		}, include.rownames=F)
		
	output$table6 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$commentsTable
		}, include.rownames=F)
		
	output$table7 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$dayVarsTable
		}, include.rownames=F)
		
	output$table8 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$focalVarsTable
		}, include.rownames=F)
		
	output$table9 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$continuousVarsTable
		}, include.rownames=F)

	output$table10 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$scanVarsTable
		}, include.rownames=F)

	output$downloadSessionsTable <- downloadHandler(
    filename = function() { 
		 paste('sessionsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$sessionsTable, file, row.names=F)
    }
  )
	output$downloadFocalsTable <- downloadHandler(
    filename = function() { 
		 paste('focalsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$focalsTable, file, row.names=F)
    }
  )
	output$downloadBehaviorsTable <- downloadHandler(
    filename = function() { 
		 paste('behaviorsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$behaviorsTable, file, row.names=F)
    }
  )
	output$downloadScansTable <- downloadHandler(
    filename = function() { 
		 paste('scansTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$scansTable, file, row.names=F)
    }
  )
  	output$downloadBackgroundTapsTable <- downloadHandler(
    filename = function() { 
		 paste('backgroundTapsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$backgroundTapsTable, file, row.names=F)
    }
  )
   	output$downloadCommentsTable <- downloadHandler(
    filename = function() { 
		 paste('commentsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$commentsTable, file, row.names=F)
    }
  )
  output$downloadDayVarsTable <- downloadHandler(
    filename = function() { 
		 paste('dayVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$dayVarsTable, file, row.names=F)
    }
  )
  output$downloadFocalVarsTable <- downloadHandler(
    filename = function() { 
		 paste('focalVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$focalVarsTable, file, row.names=F)
    }
  )
  output$downloadContinuousVarsTable <- downloadHandler(
    filename = function() { 
		 paste('continuousVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$continuousVarsTable, file, row.names=F)
    }
  )
  output$downloadScanVarsTable <- downloadHandler(
    filename = function() { 
		 paste('scanVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$scanVarsTable, file, row.names=F)
    }
  )
  
  
})
