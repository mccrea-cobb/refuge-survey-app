
##----
# Required packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(dplyr)
library(forcats)
library(stringr)
library(plotly)
library(leaflet)
library(DT)
library(RColorBrewer)
library(tidyr)
library(iris)
library(kableExtra)

##----
# Load the data from the IRIS Warehouse
# primr_dat <- iris::get_primr_iris(region = "11") %>%
#   dplyr::na_if(9999) %>%
#   dplyr::na_if(9998) %>%
#   dplyr::na_if(-9999) %>%
#   dplyr::na_if(-1) %>%
#   mutate(
#     SurveyTypeShort = forcats::fct_collapse(SurveyType,
#                                             Monitoring = c("Baseline Monitoring",
#                                                            "Coop Baseline Monitoring",
#                                                            "Monitoring to Inform Management",
#                                                            "Coop Monitoring to Inform Management"),
#                                             Inventory = c("Inventory",
#                                                           "Coop Inventory"),
#                                             Research = c("Research",
#                                                          "Coop Research")),
#     Zone = as.factor(if_else(str_detect(StationName, "Arctic|Flats|Koyukuk|Nowitna|Innoko|Selawik|Tetlin|Kanuti|Kenai"), "North", "South")),
#     Coop = as.factor(if_else(str_detect(SurveyType, "Coop"), "Cooperative", "Not Cooperative"))
#   )
#
# loc_dat <- query_iris("dbo", "DimOrganization") %>%
#   dplyr::select(RegionNumber,
#                 StationCostCenterCode = Code,
#                 Latitude,
#                 Longitude) %>%
#   dplyr::filter(RegionNumber == "11") %>%
#   dplyr::select(-RegionNumber) %>%
#   dplyr::collect()
#
# input_dat_old <- dplyr::full_join(primr_dat, loc_dat,
#                               by = "StationCostCenterCode") %>%
#   dplyr::filter(!is.na(StationName)) %>% # Removes rows containing no survey data (NAs)
#   dplyr::mutate(StationName = as.factor(stringr::str_remove(StationName,
#                                                   pattern = " National Wildlife Refuge")
#                                         ),
#                 Selected = as.factor(Selected),
#                 SurveyStatus = as.factor(SurveyStatus))
#
# # Add non-UTF-8 encoding characters in SurveyName https://stackoverflow.com/questions/17291287/how-to-identify-delete-non-utf-8-characters-in-r
# Encoding(input_dat$SurveyName) <- "UTF-8"
# input_dat$SurveyName <- iconv(input_dat$SurveyName, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
# #
# # # Save it
# # save(input_dat, file = "./data/dat.Rdata")
#
# ##----
# # Load the data using PRIMR web services (no VPN required)
# library(iris)
# library(tidyverse)
#
# input_dat <- get_primr(cost_code = "FF07%")
# input_dat <- input_dat %>%
#   dplyr::filter(!is.na(stationName)) %>% # Removes rows containing no survey data (NAs)
#   rename(StationName = stationName,
#          Selected = selected,
#          SurveyStatus = status.name,
#          SurveyName = name,
#          SurveyType = type.name,
#          Frequency = frequency.name,
#          ResourceThemeLevel1 = resourceLevel1.name,
#          ResourceThemeLevel2 = resourceLevel2.name,
#          Conducted = conducted,
#          ProtocolTitle = protocol.servCatTitle,
#          Latitude = latitude,
#          Longitude = longitude,
#          HasProtocol = protocolUsed) %>%
#   dplyr::mutate(StationName = as.factor(stringr::str_remove(StationName,
#                                                             pattern = " National Wildlife Refuge")),
#                 Selected = forcats::fct_recode(as.factor(Selected), "Yes" = "TRUE", "No" = "FALSE"),
#                 SurveyStatus = as.factor(SurveyStatus),
#                 SurveyTypeShort = forcats::fct_collapse(SurveyType,
#                                           Monitoring = c("Baseline Monitoring",
#                                                          "Coop Baseline Monitoring",
#                                                          "Monitoring to Inform Management",
#                                                          "Coop Monitoring to Inform Management"),
#                                           Inventory = c("Inventory",
#                                                         "Coop Inventory"),
#                                           Research = c("Research",
#                                                        "Coop Research")),
#                 Zone = as.factor(if_else(str_detect(StationName, "Arctic|Flats|Koyukuk|Nowitna|Innoko|Selawik|Tetlin|Kanuti|Kenai"), "North", "South")),
#                 Coop = as.factor(if_else(str_detect(SurveyType, "Coop"), "Cooperative", "Not Cooperative")),
#                 HasProtocol = forcats::fct_recode(as.factor(HasProtocol), "Has Protocol" = "TRUE", "No Protocol" = "FALSE"),
#                 ProtocolTitle = as.factor(na_if(ProtocolTitle, "NA")))
# save(input_dat, file = "./data/dat.Rdata")



##----
# Load the survey data locally
load("./data/dat.Rdata")


#-----
# Define the UI for application
ui <- function(request) {
  shiny::fluidPage(theme = shinythemes::shinytheme("cerulean"),
                   navbarPage(
                     title = "Refuge Surveys",
                     tabPanel("", icon = icon("home", lib =  "glyphicon"), value = "home",
                              fluidRow(
                                column(2,
                                       fluidRow(
                                         column(6,
                                                img(src = "aim_logo_small.png", width = 90),
                                         ),
                                         column(6,
                                                htmlOutput("dat_panel")  # Add a header
                                         )
                                       ),

                                       hr(),

                                       shinyWidgets::searchInput(inputId = "search",
                                                                 label = "Search survey titles:",
                                                                 btnSearch = icon("search"),
                                                                 btnReset = icon("remove")
                                       ),

                                       shinyWidgets::awesomeCheckboxGroup(inputId = "zone_select",
                                                                          label = "Zone:",
                                                                          choices = c("North", "South"),
                                                                          selected = NULL,
                                                                          inline = TRUE,
                                                                          status = "danger"),

                                       shinyWidgets::awesomeCheckboxGroup(inputId = "selected_select",
                                                                          label = "Selected survey?",
                                                                          choices = c("Yes", "No"),
                                                                          selected = NULL,
                                                                          inline = TRUE,
                                                                          status = "danger"),

                                       shinyWidgets::selectizeGroupUI(
                                         id = "my-filters",
                                         inline = FALSE,
                                         params = list(
                                           SurveyStatus = list(inputId = "SurveyStatus", title = "Survey Status:"),
                                           StationName = list(inputId = "StationName", title = "Refuge:"),
                                           SurveyTypeShort = list(inputId = "SurveyTypeShort", title = "Survey Type:"),
                                           Coop = list(inputId = "Coop", title = "Cooperative Survey:"),
                                           ResourceThemeLevel2 = list(inputId = "ResourceThemeLevel2", title = "Resource Theme:"),
                                           Frequency = list(inputId = "Frequency", title = "Frequency:")
                                         )
                                       ),

                                       hr(),

                                       tags$h4("Download a summary report"),
                                       downloadButton('downloadReport')
                                ),
                                column(10,
                                       tabsetPanel(
                                         tabPanel("Map",
                                                  fluidRow(
                                                    column(4,
                                                           leaflet::leafletOutput("map",
                                                                                  height = 500)
                                                    ),
                                                    column(8,
                                                           # br(),
                                                           # br(),
                                                           # br(),
                                                           DT::DTOutput("tbl_survey")  # Show a summary table of surveys
                                                    )
                                                  ),
                                                  hr(),
                                                  plotly::plotlyOutput("plot")  # Show a bar plot
                                         ),
                                         tabPanel("Surveys",

                                                  br(),

                                                  #DT::DTOutput("tbl_survey", height = 500)  # Show a summary table of surveys
                                         ),
                                         tabPanel("Protocols",

                                                  br(),

                                                  DT::DTOutput("tbl_protocol", height = 500)  # Show a summary table of protocols
                                         ),
                                         tabPanel("Annual Updates",

                                                  br(),

                                                  DT::DTOutput("tbl_conducted", height = 500))  # Show a summary table of protocols
                                       )
                                )
                              )
                     ),
                     tabPanel("Instructions",
                              "Click",
                              tags$a(href="https://github.com/mccrea-cobb/refuge-survey-app#refuge-survey-shiny-app",
                                     "here"),
                              "for instructions on how to use this app."
                              # tags$iframe(src ="https://usfws.github.io/escapement/",
                              #             height = 1200,
                              #             width = 1200,
                              #             frameborder = "no"
                              #             )
                     )
                   )
  )
}


##----
# Define the server logic
server <- function(input, output, session) {


  # Create a responsive dataset (dat()) based on user inputs in SelectizeGroupUI
  dat_filter <- callModule(
    id = "my-filters",
    module = selectizeGroupServer,
    data = input_dat,
    vars = c(
      "Zone",
      "StationName",
      "SurveyTypeShort",
      "Coop",
      "SurveyStatus",
      "Selected",
      "ResourceThemeLevel2",
      "Frequency")
  )

  dat_zone <- reactive({
    if (is.null(input$zone_select) & is.null(input$selected_select)) {
      dat_filter()
    } else if(is.null(input$selected_select)) {
      subset(dat_filter(), Zone %in% input$zone_select)
    } else if(is.null(input$zone_select)) {
      subset(dat_filter(), Selected %in% input$selected_select)
    } else dat_filter() %>% filter(Zone == input$zone_select,
                                Selected == input$selected_select)
  })

  search <- reactive({
    if (is.null(input$search)) {
      NULL
    } else {
      input$search %>%
        str_split(pattern = " ") %>%
        unlist() %>%
        paste(collapse = "|")
    }
  })

  dat <- reactive({
    if (is.null(search())) {
      dat_zone()
    } else
      dat_zone() %>%
      filter(stringr::str_detect(tolower(SurveyName), search()))
  })

##-----

  # Create a plotly plot
  output$plot <- plotly::renderPlotly({
    dat() %>%
      # Shorten refuge names and order them
      dplyr::mutate(StationName = forcats::fct_reorder(StationName, SurveyName, .fun = 'length')) %>%
      dplyr::group_by(StationName) %>%
      dplyr::count(SurveyTypeShort) %>%
      plotly::plot_ly(x = ~StationName,
                      y = ~n,
                      color = ~SurveyTypeShort) %>%
      plotly::add_bars() %>%
      plotly::layout(barmode = "stack",
                     xaxis = list(title = ""),
                     yaxis = list(title = "Surveys")
      )
  })




  # Create simplified map dataset to speed things up
  dat_surveys <- reactive({
    dat() %>%
      dplyr::select(SurveyName,
                    StationName,
                    Zone,
                    Latitude,
                    Longitude
      ) %>%
      dplyr::group_by(StationName,
                      Zone,
                      Latitude,
                      Longitude) %>%
      dplyr::count(StationName, name = "Surveys")
  })
  dat_protocols <- reactive({
    dat() %>%
      dplyr::select(StationName,
                    HasProtocol) %>%
      dplyr::group_by(StationName) %>%
      count(HasProtocol, name = "Protocols") %>%
      ungroup() %>%
      tidyr::complete(StationName, nesting(HasProtocol), fill = list(Protocols = 0)) %>%
      filter(HasProtocol == "Has Protocol") %>%
      select(!HasProtocol)
  })
  dat_map <- reactive({
    left_join(dat_surveys(), dat_protocols(), by = "StationName")
  })



  ## Create a leaflet map
  # Define the color palette for the map
  pal <- colorFactor(
    palette = c('red', 'blue'),
    domain = input_dat$Zone
  )

  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(dat_map()) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addScaleBar(position="bottomleft") %>%
      fitBounds(~min(input_dat$Longitude), ~min(input_dat$Latitude),
                ~max(input_dat$Longitude), ~max(input_dat$Latitude))
  })

  # Show points on the map that reflect user inputs
  observe({
    leaflet::leafletProxy("map", data = dat_map()) %>%
      clearShapes() %>%
      addCircleMarkers(lng = dat_map()$Longitude,
                       lat = dat_map()$Latitude,
                       radius = ~ sqrt(dat_map()$Surveys),
                       weight = 5,
                       color = ~pal(dat_map()$Zone),
                       fillColor = "#FF0000",
                       fillOpacity = 0.7,
                       popup = paste(dat_map()$StationName, "<br>",
                                     dat_map()$Surveys, "Surveys", "<br>",
                                     dat_map()$Protocols, "Protocols")) %>%
      addLegend("bottomright", pal = pal, values = ~Zone,
                title = "Refuge Zones",
                opacity = 1)
  })

  # Create a summary dataset that reacts to user inputs for the survey table
  dat_survey_tbl <- reactive({
    if (nrow(dat()) == 0) {   # if there is no data, create an empty data frame
      dat_survey_tbl <- data.frame(SurveyName = character(),
                 StationName = character(),
                 Products = character(),
                 SurveyStatus = character(),
                 SurveyTypeShort = character(),
                 Selected = character())
    } else { # Otherwise...
    dat_survey_tbl <- dat() %>%
      unnest(reports, keep_empty = TRUE) %>%
      distinct(surveyId, .keep_all = T)

    missing <- setdiff("servCatUrl", names(dat_survey_tbl))  # Check whether the servCatUrl column is missing
    if(!identical(missing, character(0))) {
      dat_survey_tbl[missing] <- "NA"  # If it is missing, create one and fill it with NAs
    }

    dat_survey_tbl <- dat_survey_tbl %>%
      mutate(Products = paste0("<a href='", servCatUrl, "' target='_blank'>", "ServCat link", "</a>")) %>%
      mutate(Products = na_if(Products, "<a href='NA' target='_blank'>ServCat link</a>")) %>%
      dplyr::select(StationName,
                    SurveyName,
                    Products,
                    SurveyStatus,
                    SurveyTypeShort,
                    Selected) %>%
      arrange(StationName, SurveyName)
    }
  })

  # Create a summary dataset that reacts to user inputs for the protocol table
  dat_protocol_tbl <- reactive({
    dat() %>%
      dplyr::select(ProtocolTitle,
                    protocol.servCatId,
                    StationName,
                    SurveyName) %>%
      filter(ProtocolTitle != "NA") %>%
      mutate(ProtocolUrl = paste0("https://ecos.fws.gov/ServCat/Reference/Profile/", protocol.servCatId)) %>%
      mutate(ProtocolTitle = paste0(ProtocolTitle, " (", paste0("<a href='", ProtocolUrl, "' target='_blank'>", "ServCat link", "</a>"), ")")) %>%
      select(ProtocolTitle, StationName, SurveyName)
  })

  # Create a summary dataset that reacts to user inputs for the annual update table
  dat_conducted_tbl <- reactive({
    if (nrow(dat()) == 0) {   # if there is no data, create an empty data frame
      dat_conducted <- data.frame(SurveyName = character(),
                                  StationName = character(),
                                  year = numeric(),
                                  isSurveyConducted = character(),
                                  reason = character(),
                                  comment = character())
    } else { # Otherwise...
      dat_conducted <- unnest(dat(), Conducted, keep_empty = TRUE)

      missing <- setdiff("year", names(dat_conducted))  # Check whether the year column is missing

      if(!identical(missing, character(0))) {
        dat_conducted <- data.frame(SurveyName = character(),
                                    StationName = character(),
                                    year = numeric(),
                                    isSurveyConducted = character(),
                                    reason = character(),
                                    comment = character())
      } else{

        dat_conducted <- dat_conducted %>% mutate(year = as.numeric(year))

        first_year <- min(dat_conducted$year, na.rm = T)
        last_year <- max(dat_conducted$year, na.rm = T)

        dat_conducted %>%
          group_by(SurveyName, StationName) %>%
          expand(year = first_year:last_year) %>%
          ungroup() %>%
          left_join(dat_conducted) %>%
          select(SurveyName, StationName, year, isSurveyConducted, reason, comment) %>%
          rename(Conducted = isSurveyConducted,
                 Reason = reason,
                 Comment = comment,
                 Year = year) %>%
          mutate(Conducted = as.factor(Conducted),
                 Year  = as.integer(Year),
                 Reason = as.factor(Reason)) %>%
          mutate(Conducted = forcats::fct_recode(Conducted, "No" = "false", "Yes" = "true"))
      }
    }
  })


  # Render the survey table using DT
  output$tbl_survey <- DT::renderDT(server = FALSE, {
    DT::datatable(
      dat_survey_tbl(),
      escape = F,
      colnames = c("Refuge",
                   "Survey Name",
                   "Products",
                   "Status",
                   "Survey Type",
                   "Selected?"),
      filter = "top",
      rownames = FALSE,
      extensions = c("RowGroup", "Buttons"),
      options = list(pageLength = 5,
                     dom = "Bfrtip",
                     rowGroup = list(dataSrc = 0),
                     columnDefs = list(list(visible = FALSE,
                                            targets = 0)),
                     buttons =
                       list(
                         list(
                           extend = "print",
                           buttons = "print",
                           exportOptions = list(
                             modifiers = list(page = "all")
                           )
                         ),
                         list(
                           extend = "excel",
                           buttons = "excel",
                           exportOptions = list(
                             modifiers = list(page = "all")
                           )
                         )
                       )
      )
    )
  })



  # Render the protocol table using DT
  output$tbl_protocol <- DT::renderDT(server = FALSE, {
    DT::datatable(
      dat_protocol_tbl(),
      escape = F,
      colnames = c("Protocol",
                   "Refuge",
                   "Survey Name"),
      filter = "top",
      rownames = FALSE,
      extensions = c("RowGroup", "Buttons"),
      options = list(pageLength = 25,
                     dom = "Bfrtip",
                     rowGroup = list(dataSrc = 0),
                     columnDefs = list(list(visible = FALSE,
                                            targets = 0)),
                     buttons =
                       list(
                         list(
                           extend = "print",
                           buttons = "print",
                           exportOptions = list(
                             modifiers = list(page = "all")
                           )
                         ),
                         list(
                           extend = "excel",
                           buttons = "excel",
                           exportOptions = list(
                             modifiers = list(page = "all")
                           )
                         )
                       )
      )
    )
  })

  # Render the annual update table using DT
  output$tbl_conducted <- DT::renderDT(server = FALSE, {
    DT::datatable(
      dat_conducted_tbl(),
      colnames = c("Survey Name",
                   "Refuge",
                   "Year",
                   "Conducted",
                   "Reason",
                   "Comment"),
      filter = "top",
      rownames = FALSE,
      extensions = c("RowGroup", "Buttons"),
      options = list(pageLength = 25,
                     dom = "Bfrtip",
                     rowGroup = list(dataSrc = 0),
                     columnDefs = list(list(visible = FALSE,
                                            targets = 0)),
                     buttons =
                       list(
                         list(
                           extend = "print",
                           buttons = "print",
                           exportOptions = list(
                             modifiers = list(page = "all")
                           )
                         ),
                         list(
                           extend = "excel",
                           buttons = "excel",
                           exportOptions = list(
                             modifiers = list(page = "all")
                           )
                         )
                       )
      )
    )
  })

  # Create html code for the summary panel
  output$dat_panel <- renderUI({
    HTML(
      paste(sep = "<br/>",
            paste("<h4>", "<b>Refuges:</b> ", nlevels(droplevels(dat()$StationName))),
            paste("<b>Surveys:</b> ", dplyr::n_distinct(dat()$SurveyName)),
            paste("<b>Protocols:</b> ", nlevels(droplevels(dat()$ProtocolTitle)), "</h4>")
      )
    )
  })

  # Set up parameters to pass to Rmd report
  params <- list(input_dat = input_dat,
                 dat = reactive(dat()),
                 zone = reactive(input$zone_select))

  # Download rmarkdown report
  output$downloadReport <- downloadHandler(
    filename = "refuge-report.html",

    content = function(file) {
      src <- normalizePath('report.Rmd')

      # Temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)

      library(rmarkdown)
      out <- rmarkdown::render('report.Rmd',
                               html_document(),
                               params = params
                               # envir = new.env(parent = globalenv())
      )
      file.rename(out, file)
    }
  )

}


# Run the application
shiny::shinyApp(ui = ui, server = server)

