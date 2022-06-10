regions <- data.frame("region_number" = c(1:8),
                      "region_name" = c("Pacific",
                                        "Southwest",
                                        "Midwest",
                                        "Southeast",
                                        "Northeast",
                                        "Mountain-Prairie",
                                        "Alaska",
                                        "Pacific Southwest"))


# Load PRIMR data from the IRIS Data Warehouse (VPN required)
primr_dat_iris <- function(){
  library(iris)
  library(tidyverse)

  primr_dat <- iris::get_primr_iris(region = "11") %>%
    dplyr::na_if(9999) %>%
    dplyr::na_if(9998) %>%
    dplyr::na_if(-9999) %>%
    dplyr::na_if(-1) %>%
    mutate(
      SurveyTypeShort = forcats::fct_collapse(SurveyType,
                                              Monitoring = c("Baseline Monitoring",
                                                             "Coop Baseline Monitoring",
                                                             "Monitoring to Inform Management",
                                                             "Coop Monitoring to Inform Management"),
                                              Inventory = c("Inventory",
                                                            "Coop Inventory"),
                                              Research = c("Research",
                                                           "Coop Research")),
      Zone = as.factor(if_else(str_detect(StationName, "Arctic|Flats|Koyukuk|Nowitna|Innoko|Selawik|Tetlin|Kanuti|Kenai"), "North", "South")),
      Coop = as.factor(if_else(str_detect(SurveyType, "Coop"), "Cooperative", "Not Cooperative"))
    )

  loc_dat <- query_iris("dbo", "DimOrganization") %>%
    dplyr::select(RegionNumber,
                  StationCostCenterCode = Code,
                  Latitude,
                  Longitude) %>%
    dplyr::filter(RegionNumber == "11") %>%
    dplyr::select(-RegionNumber) %>%
    dplyr::collect()

  input_dat_old <- dplyr::full_join(primr_dat, loc_dat,
                                    by = "StationCostCenterCode") %>%
    dplyr::filter(!is.na(StationName)) %>% # Removes rows containing no survey data (NAs)
    dplyr::mutate(StationName = as.factor(stringr::str_remove(StationName,
                                                              pattern = " National Wildlife Refuge")
    ),
    Selected = as.factor(Selected),
    SurveyStatus = as.factor(SurveyStatus))

  # Add non-UTF-8 encoding characters in SurveyName https://stackoverflow.com/questions/17291287/how-to-identify-delete-non-utf-8-characters-in-r
  Encoding(input_dat$SurveyName) <- "UTF-8"
  input_dat$SurveyName <- iconv(input_dat$SurveyName, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''

  input_dat
  # # Save it
  # save(input_dat, file = "./data/dat.Rdata")
}

##----



#' Load PRIMR data using web services (no VPN required)
#'
#' @author McCrea Cobb (mccrea_cobb@@fws.gov)
#'
#' @param region a numeric value (1-8) indicating the FWS region of interest
#'
#' @return a data frame containing PRIMR records
#' @export
#'
#' @examples
primr_dat_ws <- function(region_number){
  library(iris)
  library(tidyverse)

  region <- paste0("FF0", region_number, "%")

  input_dat <- get_primr(cost_code = region)
  input_dat <- input_dat %>%
    dplyr::filter(!is.na(stationName)) %>% # Removes rows containing no survey data (NAs)
    rename(StationName = stationName,
           Selected = selected,
           SurveyStatus = status.name,
           SurveyName = name,
           SurveyType = type.name,
           Frequency = frequency.name,
           ResourceThemeLevel1 = resourceLevel1.name,
           ResourceThemeLevel2 = resourceLevel2.name,
           Conducted = conducted,
           ProtocolTitle = protocol.servCatTitle,
           Latitude = latitude,
           Longitude = longitude,
           HasProtocol = protocolUsed) %>%
    dplyr::mutate(StationName = as.factor(stringr::str_remove(StationName,
                                                              pattern = " National Wildlife Refuge")),
                  Selected = forcats::fct_recode(as.factor(Selected), "Yes" = "TRUE", "No" = "FALSE"),
                  SurveyStatus = as.factor(SurveyStatus),
                  SurveyTypeShort = forcats::fct_collapse(SurveyType,
                                                          Monitoring = c("Baseline Monitoring",
                                                                         "Coop Baseline Monitoring",
                                                                         "Monitoring to Inform Management",
                                                                         "Coop Monitoring to Inform Management"),
                                                          Inventory = c("Inventory",
                                                                        "Coop Inventory"),
                                                          Research = c("Research",
                                                                       "Coop Research")),
                  Zone = as.factor(if_else(str_detect(StationName, "Arctic|Flats|Koyukuk|Nowitna|Innoko|Selawik|Tetlin|Kanuti|Kenai"), "North", "South")),
                  Coop = as.factor(if_else(str_detect(SurveyType, "Coop"), "Cooperative", "Not Cooperative")),
                  HasProtocol = forcats::fct_recode(as.factor(HasProtocol), "Has Protocol" = "TRUE", "No Protocol" = "FALSE"),
                  ProtocolTitle = as.factor(na_if(ProtocolTitle, "NA")))
  input_dat
  # save(input_dat, file = "./data/dat.Rdata")
}
