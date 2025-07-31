# Search tool to acquire water rights net amount from CDSS based on wdid
# Wild Animal Sanctuary

library(shiny)
library(httr2)
library(bslib)
library(rsconnect)

# Function to get CSV content from WDID
get_wdid_csv <- function(wdid) {
  base_url <- "https://dwr.state.co.us/Rest/GET/api/v2/waterrights/netamount/"
  fields <- "waterRightNetAmtNum,wdid,structureName,structureType,waterSource,gnisId,streamMile,division,waterDistrict,county,q10,q40,q160,section,township,range,pm,coordinatesEw,coordinatesEwDir,coordinatesNs,coordinatesNsDir,utmX,utmY,latitude,longitude,locationAccuracy,adjudicationDate,priorAdjudicationDate,appropriationDate,adminNumber,orderNumber,priorityNumber,associatedCaseNumbers,decreedUses,netAbsolute,netConditional,netApexAbsolute,netApexConditional,decreedUnits,seasonalLimitation,comments,lastModified,permit,moreInformation"
  
  full_url <- paste0(base_url, "?format=csv&fields=", URLencode(fields), "&wdid=", wdid)
  req <- request(full_url)
  resp <- req_perform(req)
  
  if (resp_status(resp) == 200) {
    return(resp_body_raw(resp))
  } else {
    stop(paste("Failed to retrieve data. Status code:", resp_status(resp)))
  }
}

# UI
ui <- fluidPage(theme = bs_theme(bootswatch = "darkly",
                                 bg = "#222222",
                                 fg = "#86C7ED",
                                 success ="#86C7ED"),
  titlePanel("Water Rights Net Amount CSV Downloader"),
  sidebarLayout(
    sidebarPanel(
      textInput("wdid", "Enter WDID:", value = "4400516"),
      downloadButton("download_csv", "Download CSV")
    ),
    mainPanel(
      verbatimTextOutput("status")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("wdid_", input$wdid, ".csv")
    },
    content = function(file) {
      tryCatch({
        csv_data <- get_wdid_csv(input$wdid)
        writeBin(csv_data, file)
        output$status <- renderText("✅ CSV downloaded successfully.")
      }, error = function(e) {
        output$status <- renderText(paste("❌ Error:", e$message))
      })
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)

# Create a dependency file for Connect Cloud 
rsconnect::writeManifest()