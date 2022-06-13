
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
library(reactable)
library(bookdown)
library(shinycssloaders)  # Adds a spinner progress image when tables are loading

##----
# Load the survey data locally
load("./data/input_dat.RData")


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
                                         # column(6,
                                         #        img(src = "aim_logo_small.png", width = 90),
                                         # ),
                                         column(12,
                                                htmlOutput("dat_panel")  # Add a header
                                                )
                                       ),

                                       hr(),

                                       shinyWidgets::searchInput(inputId = "search",
                                                                 label = "Search survey titles:",
                                                                 btnSearch = icon("search"),
                                                                 btnReset = icon("remove", verify_fa = FALSE)
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

                                       downloadButton('downloadReport',
                                                      label = "Summary Report")
                                ),
                                column(10,
                                       tabsetPanel(
                                         tabPanel("Search",
                                                  fluidRow(
                                                    column(4,
                                                           leaflet::leafletOutput("map",
                                                                                  height = 500)
                                                    ),
                                                    column(8,
                                                           DT::DTOutput("tbl_survey")  # Show a summary table of surveys
                                                    )
                                                  ),
                                                  hr(),
                                                  plotly::plotlyOutput("plot",
                                                                       height = "350px")  # Show a bar plot
                                         ),
                                         tabPanel("Survey Details",
                                                  # textOutput("selected2"[1])
                                                  htmlOutput("details")
                                         ),
                                         tabPanel("Protocols",

                                                  br(),

                                                  DT::DTOutput("tbl_protocol", height = 500)  # Show a summary table of protocols
                                         ),
                                         tabPanel("Annual Activity",
                                                  br(),
                                                  numericInput(inputId = "year_report",
                                                               label = "Year:",
                                                               value = as.numeric(format(Sys.Date(), "%Y"))-1,
                                                               min = 2016,
                                                               max = as.integer(format(Sys.Date(), "%Y"))-1,
                                                               step = 1,
                                                               width = '75px'),
                                                  downloadButton('downloadAnnualReport',
                                                                 label = "Annual Activity Report"),

                                                  hr(),
                                                  shinycssloaders::withSpinner(reactableOutput("tbl_conducted2")))
                                                  # DT::DTOutput("tbl_conducted", height = 500))  # Show a summary table of annual activity
                                       )
                                )
                              )
                     ),
                     tabPanel("Instructions",
                              "Click",
                              tags$a(href="https://github.com/mccrea-cobb/refuge-survey-app#instructions",
                                     "here"),
                              "for instructions on how to use this app."
                     ),
                     tabPanel("Submit an Issue",
                              "Click",
                              tags$a(href="https://github.com/mccrea-cobb/refuge-survey-app/issues/new",
                                     "here"),
                              "to submit a feature request or report a bug through GitHub.",
                              br(), br(),
                              "You can also contact the developer:",
                              br(),
                              "McCrea Cobb ",
                              tags$a(href="mailto:mccrea.cobb@fws.gov",
                                     "mccrea_cobb@fws.gov"),
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
    inline = FALSE,
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
      dat_zone() %>%
        arrange(StationName, SurveyName)

    } else
      dat_zone() %>%
      # filter(if_any(everything(), ~str_detect(tolower(.), search()))) %>%  # search all columns
      filter(stringr::str_detect(tolower(SurveyName), search())) %>%  # search just the title
      arrange(StationName, SurveyName)

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
                       fillColor = ~pal(dat_map()$Zone),
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
      mutate(Products = ifelse(is.na(servCatUrl), NA, paste0("<a href='", servCatUrl, "' target='_blank'>", as.character(icon("link", lib = "glyphicon")), "</a>"))) %>%
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
      mutate(ProtocolTitle = paste0(ProtocolTitle, " (", paste0("<a href='", ProtocolUrl, "' target='_blank'", as.character(icon("link", lib = "glyphicon")), "</a>"), ")")) %>%
      select(ProtocolTitle, StationName, SurveyName) %>%
      arrange(ProtocolTitle)
  })



  # Create a summary dataset that reacts to user inputs for the annual update table
  dat_conducted_tbl <- reactive({
    if (nrow(dat()) == 0) {   # if there is no data, create an empty data frame
      dat_conducted <- data.frame(SurveyName = character(),
                                  StationName = character(),
                                  Year = numeric(),
                                  Scheduled = character(),
                                  Conducted = character(),
                                  Reason = character(),
                                  Comment = character())
    } else { # Otherwise...
      dat_conducted <- unnest(dat(), Conducted, keep_empty = TRUE)

      missing <- setdiff("year", names(dat_conducted))  # Check whether the year column is missing

      if(!identical(missing, character(0))) {
        dat_conducted <- data.frame(SurveyName = character(),
                                    StationName = character(),
                                    Year = numeric(),
                                    Scheduled = character(),
                                    Conducted = character(),
                                    Reason = character(),
                                    Comment = character())
      } else {

        dat_conducted <- dat_conducted %>%
          mutate(year = as.numeric(year)) %>%
          filter(startYear != "Future/TBD") %>%  # filter out future surveys
          mutate(startYear = as.numeric(startYear)) %>%
          filter(startYear <= format(Sys.Date(), "%Y")) %>%  # filter out surveys starting after current year
          mutate(year = ifelse(is.na(year), startYear, year), # Make startYear the year value for surveys with no annual updates (those with NAs)
                 FrequencyNum = ifelse(Frequency == "Recurring -- every year" | Frequency == "Sporadic or Ad Hoc" | Frequency == "Occurs one time only", 1, # Create a numeric frequency value
                                       ifelse(Frequency == "Recurring -- every two years", 2,
                                              ifelse(Frequency == "Recurring -- every three years", 3,
                                                     ifelse(Frequency == "Recurring -- every five years", 5,
                                                            ifelse(Frequency == "Recurring -- every decade", 10, 0))))))

        dat_conducted <- dat_conducted %>%
          group_by(SurveyName, StationName, FrequencyNum, startYear) %>%
          expand(year = dat_conducted$startYear:ifelse(endYear == "Indefinite" | endYear > as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(Sys.Date(), "%Y")), as.numeric(endYear))) %>%
          ungroup() %>%
          left_join(dat_conducted) %>%
          select(SurveyName, StationName, FrequencyNum, startYear, year, isSurveyConducted, reason, comment) %>%
          rename(Conducted = isSurveyConducted,
                 Reason = reason,
                 Comment = comment,
                 Year = year) %>%
          mutate(YearsSinceStart = Year - startYear,
                 Scheduled = YearsSinceStart %% FrequencyNum == 0,
                 Conducted = as.factor(Conducted),
                 Year = as.factor(Year),
                 Reason = as.factor(Reason),
                 Conducted = forcats::fct_recode(Conducted, "No" = "false", "Yes" = "true"),
                 Scheduled = forcats::fct_recode(as.factor(Scheduled), "No" = "FALSE", "Yes" = "TRUE")) %>%
          select(-c(FrequencyNum, YearsSinceStart, startYear)) %>%
          relocate(StationName, SurveyName, Year, Scheduled, Conducted, Reason, Comment) %>%
          filter(Conducted == "Yes" | Conducted  == "No") %>%
          arrange(StationName)
      }
    }
  })



  # Render the survey table using DT
  output$tbl_survey <- DT::renderDT(server = FALSE, {
    DT::datatable(
      dat_survey_tbl(),
      escape = F,
      colnames = c("Refuge",
                   "Survey",
                   "Products",
                   "Status",
                   "Survey Type",
                   "Selected?"),
      filter = "top",
      rownames = FALSE,
      selection = "single",
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



  # Create text in the Details Tab from the selected row in tbl_survey
  selected <- reactive({
    if(input$tbl_survey_rows_selected) {
    selectedrow <- input$tbl_survey_rows_selected
    selected <- dat() %>%
      slice(selectedrow)
    } else selected <- NA
  })
  output$details <- renderUI({
    validate(need(input$tbl_survey_rows_selected, "Select a survey (row) from the table in the Search Tab."))
    HTML(
      paste(
        sep = "<br>",
        paste0("<center><h2><b>", selected()[["SurveyName"]], "</b></h2></center>"),
        paste0("<center><h4><b>", selected()[["StationName"]], "</b></h4></center>", "<br>"),
        paste0("<b>Survey Coordinator: </b>", selected()[["coordinatorName"]], ", ", selected()[["coordinatorTitle"]], " (", selected()[["coordinatorEmail"]], ")", "<br>"),
        paste0("<b>Years: </b>", selected()[["startYear"]], "-", selected()[["endYear"]], "<br>"),
        paste0("<b>Products: </b>", selected()[["reports"]][[1]]$servCatUrl, "<br>"),
        paste0("<b>Protocol: </b>", as.character(selected()[["ProtocolTitle"]]))
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
                   "Survey"),
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
      colnames = c("Refuge",
                   "Survey",
                   "Year",
                   "Scheduled",
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

  dat_conducted <- reactive({
    dat() %>%
    unnest(Conducted)
  })

  data <- reactive({
    unique(dat_conducted_tbl()[, c("StationName", "SurveyName")])
  })

  output$tbl_conducted2 <- renderReactable({
    reactable(data(),
              sortable = T,
              filterable = T,
              groupBy = "StationName",
              columns = list(
                StationName = colDef(name = "Refuge"),
                SurveyName = colDef(name = "Survey")),
              details = function(index) {
                tbl_details <- dat_conducted_tbl()[dat_conducted_tbl()$SurveyName == dat_conducted_tbl()$SurveyName[index], ]
                htmltools::div(style = "padding: 16px",
                               reactable(tbl_details[, c("Year", "Conducted", "Reason", "Comment")],
                                         # columns = list(
                                         #   year = colDef(name = "Year"),
                                         #   isSurveyConducted = colDef(name = "Conducted?"),
                                         #   reason = colDef(name = "Reason"),
                                         #   comment = colDef(name = "Comment")),
                                         outlined = FALSE)
                )
              })
  })

  # Create html code for the summary panel
  output$dat_panel <- renderUI({
    HTML(
      paste(sep = "<br/>",
            paste("<h4>", "<font color='red'>", "<b>Refuges:</b> ", nlevels(droplevels(dat()$StationName))),
            paste("<b>Surveys:</b> ", nrow(dat())),
            paste("<b>Protocols:</b> ", nlevels(droplevels(dat()$ProtocolTitle)), "</h4>")
      )
    )
  })



  # Set up parameters to pass to Summary Report
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

  # Set up parameters to pass to Annual Report
  params_annual <- list(
    year = isolate(input$year_report))

  # Download rmarkdown annual report
  output$downloadAnnualReport <- downloadHandler(
    filename = "annual-report.pdf",

    content = function(file) {
      src_rmd <- normalizePath('annual_report.Rmd')
      src_latex <- normalizePath("latex")
      src_images <- normalizePath("images")


      # Temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src_rmd, 'annual_report.Rmd', overwrite = TRUE)
      R.utils::copyDirectory(src_latex, "latex")
      R.utils::copyDirectory(src_images, "images")

      withProgress(message = "This takes a minute or two", detail = "Your report is rendering...", {
        out <- rmarkdown::render('annual_report.Rmd',
                                 params = params_annual
                                 # envir = new.env(parent = globalenv())
        )
      })
      file.rename(out, file)
    }
  )

}


# Run the application
shiny::shinyApp(ui = ui, server = server)

