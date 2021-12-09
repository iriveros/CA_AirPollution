if(!require(shiny)) install.packages("shiny") 
if(!require(shinythemes)) install.packages("shinythemes") 
if(!require(DT)) install.packages("DT")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(shinycssloaders)) install.packages("shinycssloaders")
if(!require(maps)) install.packages("maps")
if(!require(thematic)) install.packages("thematic")
if(!require(hash)) install.packages("hash")
if(!require(sp)) install.packages("sp")
if(!require(tools)) install.packages("tools")

lastUpdate <- "None"
last_interacted <- list(NULL, "None")

CategoricalValuesList <-
  c(
    "Site",
    "Program",
    "Analytical Method",
    "Collection Method",
    "Year",
    "Time",
    "Pollutant"
  )
CategoricalValuesListNonP <-
  c("Site",
    "Program",
    "Analytical Method",
    "Collection Method",
    "Year",
    "Time")
CategoricalValuesListPureCat <-
  c("Site",
    "Program",
    "Analytical Method",
    "Collection Method",
    "Pollutant")
CategoricalValuesListPureCatNoP <-
  c("Site", "Program", "Analytical Method", "Collection Method")
CategoricalValuesListPlural <-
  c(
    "Sites",
    "Programs",
    "Analytical Methods",
    "Collection Methods",
    "Years",
    "Times",
    "Pollutants"
  )
col_conv <-
  hash(
    c(CategoricalValuesList, "Date", CategoricalValuesListPlural),
    c(
      "AMA_SITE_CODE",
      "PROGRAM",
      "SAMPLE_ANALYSIS_DESC",
      "SAMPLE_COLLECTION_DESC",
      "YEAR",
      "SAMPLE_START_TIME",
      "AQS_PARAMETER_NAME",
      "SAMPLE_DATE",
      "AMA_SITE_CODE",
      "PROGRAM",
      "SAMPLE_ANALYSIS_DESC",
      "SAMPLE_COLLECTION_DESC",
      "YEAR",
      "SAMPLE_START_TIME",
      "AQS_PARAMETER_NAME"
    )
  )