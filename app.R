
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

##----
# # Load the data from the IRIS Warehouse
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
#     Zone = if_else(str_detect(StationName, "Arctic|Flats|Koyukuk|Nowitna|Selawik|Tetlin|Kanuti"), "North", "South"),
#     Coop = if_else(str_detect(SurveyType, "Coop"), "Cooperative", "Not Cooperative")
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
# input_dat <- dplyr::full_join(primr_dat, loc_dat,
#                               by = "StationCostCenterCode") %>%
#   dplyr::filter(!is.na(StationName)) %>% # Removes rows containing no survey data (NAs)
#   dplyr::mutate(StationName = as.factor(stringr::str_remove(StationName,
#                                                   pattern = " National Wildlife Refuge")))


  # Load the survey data locally
  load("./data/dat.Rdata")


#-----
# Define the UI for application
ui <- function(request) {
  shiny::fluidPage(theme = shinythemes::shinytheme("cerulean"),
                   navbarPage(
                     title = "Refuge Surveys",
                     tabPanel("Home",
                              fluidRow(
                                column(2,
                                       img(src = "aim_logo_small.png", width = 150),

                                       br(), br(),

                                       # bookmarkButton(),

                                       # actionButton("north", "Doug"),
                                       # actionButton("south", "Ronnie"),

                                       htmlOutput("dat_panel"),  # Add a header

                                       br(),

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
                                           #Zone = list(inputId = "Zone", title = "Refuge Zone:"),
                                           StationName = list(inputId = "StationName", title = "Refuge:"),
                                           SurveyTypeShort = list(inputId = "SurveyTypeShort", title = "Survey Type:"),
                                           Coop = list(inputId = "Coop", title = "Cooperative Survey:"),
                                           SurveyStatus = list(inputId = "SurveyStatus", title = "Survey Status:"),
                                           # Selected = list(inputId = "Selected", title = "Selected Survey:"),
                                           ResourceThemeLevel2 = list(inputId = "ResourceThemeLevel2", title = "Resource Theme:")
                                         )
                                       ),

                                       radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                                    inline = TRUE),
                                       downloadButton('downloadReport')

                                       #tags$h4("Hover over the plot for more details"),
                                ),
                                column(10,
                                       tabsetPanel(
                                         tabPanel("Map",

                                                  #tags$h4("Click on a point for details on a refuge"),
                                                  leaflet::leafletOutput("map",
                                                                         height = 500),

                                                  plotly::plotlyOutput("plot")  # Show a bar plot
                                         ),
                                         tabPanel("Surveys",
                                                  img(src='https://imgs.xkcd.com/comics/self_description.png', "https://xkcd.com/688/", align = "center"),

                                                  br(), br(),

                                                  DT::DTOutput("tbl_survey", height = 500)  # Show a summary table of surveys
                                         ),
                                         tabPanel("Protocols",
                                                  DT::DTOutput("tbl_protocol", height = 500))  # Show a summary table of surveys)
                                       )
                                )
                              )
                     )
                   )
  )
}


##----
# Define the server logic
server <- function(input, output, session) {

  dat_zone <- reactive({
    if(is.null(input$zone_select) & is.null(input$selected_select)) {
      input_dat
    } else if(is.null(input$selected_select)) {
      subset(input_dat, Zone %in% input$zone_select)
    } else if(is.null(input$zone_select)) {
      subset(input_dat, Selected %in% input$selected_select)
    } else input_dat %>% filter(Zone == input$zone_select,
                                Selected == input$selected_select)
  })

  dat_search <- reactive({
    if(!is.null(input$search)) {
      dat_zone() %>%
        filter(stringr::str_detect(SurveyName, input$search))
    } else dat_zone()
  })

  # Create a responsive dataset (dat()) based on user inputs in SelectizeGroupUI
  dat <- callModule(
    id = "my-filters",
    module = selectizeGroupServer,
    data = dat_search,
    vars = c(
      "Zone",
      "StationName",
      "SurveyTypeShort",
      "Coop",
      "SurveyStatus",
      "Selected",
      "ResourceThemeLevel2")
  )

  # Create buttons for North and South Zone settings
  # observeEvent(input$north, {
  #   updateCheckboxGroupInput(
  #     session,
  #     inputId = "zone_select",
  #     selected = "North"
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-SurveyStatus",
  #     selected = c("Current", "Expected")
  #   )
  #   updateCheckboxGroupInput(
  #     session,
  #     inputId = "selected_select",
  #     selected = "Yes"
  #   )
  # })
  #
  # observeEvent(input$south, {
  #   updateCheckboxGroupInput(
  #     session,
  #     inputId = "zone_select",
  #     selected = "South"
  #   )
  #
  #   updateCheckboxGroupInput(
  #     session,
  #     inputId = "my-filters-Selected",
  #     selected = "Yes"
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-SurveyStatus",
  #     selected = c("Current", "Expected")
  #   )
  # })


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
                     yaxis = list(title = "")
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
    full_join(dat_surveys(), dat_protocols(), by = "StationName")
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
    dat() %>%
      dplyr::select(StationName,
                    SurveyName,
                    SurveyStatus,
                    Selected) %>%
      arrange(StationName, SurveyName, SurveyStatus)
  })

  # Create a summary dataset that reacts to user inputs for the protocol table
  dat_protocol_tbl <- reactive({
    dat() %>%
      dplyr::select(ProtocolTitle,
                    StationName,
                    SurveyName) %>%
      filter(ProtocolTitle != "NA") %>%
      arrange(ProtocolTitle, StationName, SurveyName)
  })

  # Render the survey table using DT
  output$tbl_survey <- DT::renderDataTable(dat_survey_tbl(),
                                           colnames = c("Refuge",
                                                        "Survey Name",
                                                        "Status",
                                                        "Selected?"),
                                           options = list(pageLength = 25)
  )

  # Render the protocol table using DT
  output$tbl_protocol <- DT::renderDataTable(dat_protocol_tbl(),
                                             colnames = c("Protocol",
                                                          "Refuge",
                                                          "Survey Name"),
                                             options = list(pageLength = 25)
  )

  # Create html code for the summary panel
  output$dat_panel <- renderUI({
    HTML(
      paste(sep = "<br/>",
            paste("<h4>", "<b>Refuges:</b> ", nrow(dat_map())),
            paste("<b>Surveys:</b> ", sum(dat_map()$Surveys)),
            paste("<b>Protocols:</b> ", n_distinct(dat()$ProtocolTitle) - 1, "</h4>")
      )
    )
  })

  # Download rmarkdown report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      src <- normalizePath('report.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)

      library(rmarkdown)
      out <- rmarkdown::render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ),
      params = list(data = reactive(dat())))
      file.rename(out, file)
    }
  )

}


# Run the application
shiny::shinyApp(ui = ui, server = server, enableBookmarking = "url")

