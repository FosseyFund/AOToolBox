fluidPage(   
    sidebarLayout(
    sidebarPanel(
    #helpText("Upload collected data"),
    fileInput('json.output.file', '1. Upload "Username_Date_Time.dat"'),
	fileInput('behaviors.json', '2. Upload "behaviors.json"'),
    fileInput('layout_info.json', '3. Upload "layout_info.json"'),
    checkboxInput("colmerge", "Merge identically-named columns from dyadic and self-directed/health data", FALSE),
	    HTML('<hr style="height:1px;border:none;color:#333;background-color:#333;" />'),
	    actionButton("reacButton", label = "Press here!"),
	    fluidRow(column(3, verbatimTextOutput("value")))),

     mainPanel(
      br()
  	
)
)
)


