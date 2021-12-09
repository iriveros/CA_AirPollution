library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(shinycssloaders)
library(maps)
library(thematic)
library(hash)
library(sp)
library(tools)
library(DT)

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