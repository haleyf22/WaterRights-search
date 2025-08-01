# Search tool to acquire water rights net amount from CDSS based on wdid
# Wild Animal Sanctuary

library(shiny)
library(httr2)
library(bslib)
library(rsconnect)
library(readr)  
library(dplyr)
library(jsonlite)


# Function to download raw data for a single WDID and return as a data.frame

# Water Rights Net Amount
get_wdid_df <- function(wdid) {
  base_url <- "https://dwr.state.co.us/Rest/GET/api/v2/waterrights/netamount/"
  fields <- "waterRightNetAmtNum,wdid,structureName,structureType,waterSource,gnisId,streamMile,division,waterDistrict,county,q10,q40,q160,section,township,range,pm,coordinatesEw,coordinatesEwDir,coordinatesNs,utmX,utmY,latitude,longitude,locationAccuracy,adjudicationDate,priorAdjudicationDate,appropriationDate,adminNumber,orderNumber,priorityNumber,associatedCaseNumbers,decreedUses,netAbsolute,netConditional,netApexAbsolute,netApexConditional,decreedUnits,seasonalLimitation,comments,lastModified,permit,moreInformation"
  
  full_url <- paste0(base_url, "?format=csv&fields=", URLencode(fields), "&wdid=", wdid)
  req <- request(full_url)
  resp <- req_perform(req)
  
  if (resp_status(resp) == 200) {
    text_resp <- rawToChar(resp_body_raw(resp))
    if (grepl("wdid,", text_resp)) {
      df <- read_csv(text_resp, skip = 2, show_col_types = FALSE)
      
      # remove any rows where first column equals its name (header as row)
      header_names <- names(df)
      
      # remove rows where the first column matches its name ("wdid")
      df <- df[df[[1]] != "1", ]
      df <- df[!apply(df, 1, function(row) all(row == header_names)), ]
      
      return(df)
    } else {
      stop(paste("No data returned for WDID:", wdid))
    }
  } else {
    stop(paste("Request failed for WDID:", wdid, "Status code:", resp_status(resp)))
  }
}

# Structure
get_structure_df <- function(wdid) {
  base_url <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/"
  fields <- "wdid,structureName,associatedAkas,ciuCode,structureType,waterSource,gnisId,streamMile,associatedCaseNumbers,associatedPermits,associatedMeters,associatedContacts,porStart,porEnd,division,waterDistrict,subdistrict,county,designatedBasinName,managementDistrictName,pm,township,range,section,q10,q40,q160,coordsew,coordsewDir,coordsns,coordsnsDir,utmX,utmY,latdecdeg,longdecdeg,locationAccuracy,modified"
  
  full_url <- paste0(base_url, "?format=csv&fields=", URLencode(fields), "&wdid=", wdid)
  req <- request(full_url)
  resp <- req_perform(req)
  
  if (resp_status(resp) == 200) {
    text_resp <- rawToChar(resp_body_raw(resp))
    if (grepl("wdid,", text_resp)) {
      df <- read_csv(text_resp, skip = 2, show_col_types = FALSE)
      
      # remove any rows where first column equals its name (header as row)
      header_names <- names(df)
      
      # remove rows where the first column matches its name ("wdid")
      #df <- df[df[[1]] != "1", ]
      df <- df[!apply(df, 1, function(row) all(row == header_names)), ]
      
      return(df)
    } else {
      stop(paste("No data returned for WDID:", wdid))
    }
  } else {
    stop(paste("Request failed for WDID:", wdid, "Status code:", resp_status(resp)))
  }
}

# -----UI-----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly", bg = "#222222", fg = "#86C7ED", success = "#86C7ED"),
  titlePanel("CO DWR Net Amount & Structure CSV Downloader"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("search_type", "Search type:",
                   choices = c("Water Rights Net Amount" = "netamt",
                               "Structure Search" = "structure"),
                   selected = "netamt"),
      textAreaInput("wdid", "Enter WDID(s), separated by commas and no spaces:", value = "4404516"),
      downloadButton("download_csv", "Download Combined CSV")
    ),
    mainPanel(
      verbatimTextOutput("status")
    )
  )
)


# -----Server-----
server <- function(input, output, session) {
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("wdids_", input$search_type, "_combined_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      wdid_list <- unlist(strsplit(isolate(input$wdid), "[,\\s]+"))
      wdid_list <- wdid_list[wdid_list != ""]
      
      all_results <- list()
      errors <- c()
      
      for (wdid in wdid_list) {
        tryCatch({
          if (input$search_type == "netamt") {
            df <- get_wdid_df(wdid)
          } else {
            df <- get_structure_df(wdid)
          }
          all_results[[wdid]] <- df
        }, error = function(e) {
          errors <<- c(errors, paste("WDID", wdid, ":", e$message))
        })
      }
      
      if (length(all_results) == 0) {
        output$status <- renderText(paste("❌ No valid data downloaded.", paste(errors, collapse = "; ")))
        writeLines("No data downloaded.", file)
      } else {
        combined <- dplyr::bind_rows(all_results)
        readr::write_csv(combined, file)
        msg <- paste0("✅ Downloaded ", length(all_results), " WDID(s).")
        if (length(errors) > 0) {
          msg <- paste0(msg, "\nErrors:\n", paste(errors, collapse = "\n"))
        }
        output$status <- renderText(msg)
      }
    }
  )
}


rsconnect::writeManifest()

shinyApp(ui = ui, server = server)