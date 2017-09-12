shinyUI(fluidPage(   
    sidebarLayout(
    sidebarPanel(
    #helpText("Upload collected data"),
    fileInput('json.output.file', '1. Upload "Username_Date_Time.dat"'),
	fileInput('behaviors.json', '2. Upload "behaviors.json"'),
    fileInput('layout_info.json', '3. Upload "layout_info.json"'),
    checkboxInput("colmerge", "Merge identically-named columns from dyadic and self-directed/health data", FALSE),
	    HTML('<hr style="height:1px;border:none;color:#333;background-color:#333;" />'),
	    actionButton("reacButton", label = "Press here!"),
    downloadButton("downloadZip", label = "Download all data as zip"),
    br(), br(), h5("    OR"), br(),      
      downloadButton('downloadSessionsTable', 'Download list of sessions'),
      downloadButton('downloadFocalsTable', 'Download list of focals'),
      downloadButton('downloadBehaviorsTable', 'Download list of behaviors'),
	  downloadButton('downloadScansTable', 'Download list of scans'),
	  downloadButton('downloadBackgroundTapsTable', 'Download list of background taps'),
	  downloadButton('downloadCommentsTable', 'Download list of comments'),
	  downloadButton('downloadDayVarsTable', 'Download day variables'),
	  downloadButton('downloadFocalVarsTable', 'Download focal variables'),
	  downloadButton('downloadContinuousVarsTable', 'Download global variables'),
  	  downloadButton('downloadScanVarsTable', 'Download scan variables')
    ),
    mainPanel(
      br()
  	
)
)
)
)
