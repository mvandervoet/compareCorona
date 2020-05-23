# Project: Compare Corona virus outbreak between countries
# Author: Monique van der Voet

library("dplyr")
library("shiny")
library("shinyWidgets")

# Load lookup tables
appSelectizeLookupNL <-
  readr::read_csv("data/appSelectizeLookupNL.csv")
appSelectizeLookupCN <-
  readr::read_csv("data/appSelectizeLookupCN.csv")
appSelectizeLookupUK <-
  readr::read_csv("data/appSelectizeLookupUK.csv")
appSelectizeLookupUS <-
  readr::read_csv("data/appSelectizeLookupUS.csv")

server <- function(input, output, session) {
  
  # Create Selectize toggle box ---------------------------------------------------------------------------------------
  
  # Select country
  updateSelectizeInput(session,
                       "ref_id",
                       choices = appSelectizeLookupNL,
                       server = TRUE)
  
  output$values_ref_id <- renderText({
    paste("You chose: ", input$ref_id)  # This text is printed in the app
  })
  
  updateSelectizeInput(session,
                       "query_id",
                       choices = appSelectizeLookupUK,
                       server = TRUE)
  
  output$values_query_id <- renderText({
    paste("You chose: ", input$query_id)  # This text is printed in the app
  })
  
  updateSelectizeInput(session,
                       "query2_id",
                       choices = appSelectizeLookupUS,
                       server = TRUE)
  
  output$values_query_id <- renderText({
    paste("You chose: ", input$query2_id)  # This text is printed in the app
  })
  
  # Select date
  updateSelectizeInput(session,
                       "ref_date",
                       choices = data.frame(value = c(seq(
                         as.Date("2019-12-30"), as.Date(format(Sys.time(), "%Y-%m-%d")), "days"
                       )),
                       label = c(seq(
                         as.Date("2019-12-30"), as.Date(format(Sys.time(), "%Y-%m-%d")), "days"
                       ))),
                       server = TRUE)
  
  output$values_ref_date <- renderText({
    paste("You chose: ", input$ref_date)  # This text is printed in the app
  })
  
  updateSelectizeInput(session,
                       "query_date",
                       choices = data.frame(value = c(seq(
                         as.Date("2019-12-30"), as.Date(format(Sys.time(), "%Y-%m-%d")), "days"
                       )),
                       label = c(seq(
                         as.Date("2019-12-30"), as.Date(format(Sys.time(), "%Y-%m-%d")), "days"
                       ))),
                       server = TRUE)
  
  output$values_query_date <- renderText({
    paste("You chose: ", input$query_date)  # This text is printed in the app
  })
  
  updateSelectizeInput(
    session,
    "query2_date",
    choices = data.frame(value = c(seq(
      as.Date("2019-12-30"), as.Date(format(Sys.time(), "%Y-%m-%d")), "days"
    )),
    label = c(seq(
      as.Date("2019-12-30"), as.Date(format(Sys.time(), "%Y-%m-%d")), "days"
    ))),
    server = TRUE
  )
  
  output$values_query2_date <- renderText({
    paste("You chose: ", input$query2_date)  # This text is printed in the app
  })
  
  # Create Graph ------------------------------------------------------------------------------------------------------
  
  observeEvent(input$goCompare, {
    showNotification(
      "I'm working on your query! This will take a few seconds.",
      duration = 2,
      type = "message"
    )
    
    input$ref_id
    input$query_id
    input$query2_id
    input$ref_date
    input$query_date
    input$query2_date
    
    # Check boxes
    input$check100k
    input$check7day
    
    output$plotCompare <- renderPlot({
      req(input$ref_id)
      req(input$query_id)
      req(input$query2_id)
      req(input$ref_date)
      req(input$query_date)
      req(input$query2_date)
      
      # Define Variables
      outbreakCurrent <- input$ref_id # "NL"
      outbreakPast <- input$query_id # "CN"
      outbreakPast2 <- input$query2_id # "IT"
      
      outbreakCurrentlockdown <-
        input$ref_date  %>% as.Date() # "2020-03-15"
      outbreakPastlockdown <-
        input$query_date  %>% as.Date() # "2020-01-23"
      outbreakPastlockdown2 <-
        input$query2_date  %>% as.Date() # "2020-03-09"
      
      # Create the URL where the dataset is stored with automatic updates every day -----------------------------------
      
      urlToday <-
        paste(
          "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
          format(Sys.time(), "%Y-%m-%d"),
          ".xlsx",
          sep = ""
        )
      urlYesterday <-
        paste(
          "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
          format(Sys.Date() - 1, "%Y-%m-%d"),
          ".xlsx",
          sep = ""
        )
      
      # Check status code, whether a current daily update is available otherwise take url from previous day
      response <-
        httr::GET(urlToday, httr::authenticate(":", ":", type = "ntlm"))
      if (response$status_code == 200) {
        url <- urlToday
        timestamp <- format(Sys.time(), "%Y-%m-%d")
      } else {
        url <- urlYesterday
        timestamp <- format(Sys.Date() - 1, "%Y-%m-%d")
      }
      
      # Download the dataset from the website to a local temporary file
      response <-
        httr::GET(
          url,
          httr::authenticate(":", ":", type = "ntlm"),
          httr::write_disk(tf <- tempfile(fileext = ".xlsx"))
        )
      
      # Load the full dataset
      data <- readxl::read_excel(tf) %>%
        mutate(dateRep = (dateRep - lubridate::days(1))) # correct for a one-day delay in reporting
      names(data)[names(data) == "countriesAndTerritories"] <-
        "Country"
      
      # Process date, comparing key event dates
      datasubset <-
        subset(data,
               geoId %in% c(outbreakCurrent, outbreakPast, outbreakPast2))
      offset <-
        difftime(outbreakCurrentlockdown, outbreakPastlockdown, units = "days") %>% as.numeric
      offset2 <-
        difftime(outbreakCurrentlockdown, outbreakPastlockdown2, units = "days") %>% as.numeric
      
      datasubset[datasubset$geoId == outbreakPast, ] <-
        mutate(datasubset[datasubset$geoId == outbreakPast, ], dateRep = (dateRep + lubridate::days(offset)))
      
      datasubset[datasubset$geoId == outbreakPast2, ] <-
        mutate(datasubset[datasubset$geoId == outbreakPast2, ], dateRep = (dateRep + lubridate::days(offset2)))
      
      datasubset$Date <- as.Date(datasubset$dateRep)
      
      # Process Corona metric, per 100k and moving average
      datasubset <- datasubset %>%
        select(Country, dateRep, coronaMetric = input$metric, popData2018)
      
      if(input$check100k & input$check7day) {
        output <- datasubset %>%
              dplyr::group_by(Country) %>%
              # coronaMetric/100k population
              mutate(coronaMetric = coronaMetric / popData2018 * 100000) %>%
              # Moving average
              mutate(coronaMetric = zoo::rollmean(coronaMetric, k = 7, fill = NA))
      } else if (input$check100k) {
        output <- datasubset %>%
          dplyr::group_by(Country) %>%
          # coronaMetric/100k population
          mutate(coronaMetric = coronaMetric / popData2018 * 100000)
      } else if (input$check7day) {
        output <- datasubset %>%
          dplyr::group_by(Country) %>%
          # Moving average
          mutate(coronaMetric = zoo::rollmean(coronaMetric, k = 7, fill = NA))
      } else {
        output <- datasubset
      }
      
      
      
      plot <- ggpubr::ggline(
        output,
        x = "dateRep",
        y = "coronaMetric",
        point.size = 0.1,
        #xscale = ,
        color = "Country",
        #palette = c("#E7B800", "#00AFBB"),
        xlab = paste(
          "\nDate (",
          outbreakPast,
          " was offset by ",
          offset,
          " days, ",
          outbreakPast2,
          " was offset by ",
          offset2,
          " days) \n\nupdated: ",
          timestamp,
          sep = ""
        ),
        ylab = paste0(input$metric, " / day\n")
      )
      plot
      
    }, )
    
  })
  
  # End
}
