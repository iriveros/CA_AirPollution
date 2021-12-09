source("global.R", local = TRUE)

get_county_verts <- function(m, s)
{
  h <- hash()
  for(county in s)
  {
    pointsDF <- filter(m, subregion == county) %>% select("long", "lat")
    h[county] <- data.matrix(pointsDF)
  }
  return(h)
}

get_county_pip_init <- function(long, lat, c, cH)
{
  r_counties <- vector(mode = "character", length = length(long))
  for(county in c$subregion)
  {
    matrix_c <- cH[[county]]
    num_mat <- point.in.polygon(long, lat, matrix_c[,1], matrix_c[,2])
    r_counties <- ifelse(num_mat != 0, c(r_counties, county), NA)
  }
  return(r_counties)
}

get_county_pip <- function(click_mat, c, cH)
{
  for(county in c$subregion)
  {
    matrix_c <- cH[[county]]
    #print(point.in.polygon(click_mat[,1], click_mat[,2], matrix_c[,1], matrix_c[,2]))
    if(point.in.polygon(click_mat[,1], click_mat[,2], matrix_c[,1], matrix_c[,2]))
    {
      return(county)
    }
  }
  return("NiC")
}

inputEventHandlerGG <- function(mySearch, myName, myCol, filterDF, session)
{
  vals <- isolate(mySearch)
  if(is.null(isolate(mySearch)))
  {
    myDF <- distinct(filterDF, !!as.symbol(myCol))
    updateSelectizeInput(session, myName, choices = as.list(myDF[,1]), server = TRUE)
  }
  else
  {
    filterDF <- filter(filterDF, !!as.symbol(myCol) %in% vals)
  }
  return(filterDF)
}

initialize_load_data <- function (air_pollution)
{
  file.list <- vector()
  for(i in 0:21)
  {
    file.list <- c(file.list, paste(getwd(),"/AMBIENT_MONITORING_ARICHIVE_FILES/AMBIENT_MONITORING_ARCHIVE_OUTPUT_", i, ".csv", sep =""))
  }
  print(file.list)
  air_pollution<-do.call(rbind, lapply(file.list, function(x){read.csv(x,FALSE); }))
  colnames(air_pollution) <- c("STATE_ABBR", "AMA_SITE_CODE", "AQS_POC", "PROGRAM", "YEAR", "QUARTER", "SAMPLE_DATE", "SAMPLE_START_TIME", "AQS_PARAMETER_CODE", 
                             "AQS_PARAMETER_NAME", "DATA_SOURCE", "DURATION_DESC", "SAMPLE_VALUE_REPORTED", "AQS_UNIT_CODE", "UNIT_DESC ", "SAMPLING_FREQUENCY_CODE",
                             "COMMENT", "SAMPLE_VALUE_STD", "SAMPLE_VALUE_STD_FINAL_UG_M3", "SAMPLE_VALUE_STD_FINAL_TYPE", "AQS_PARAMETER_CODE_FINAL", 
                             "AQS_PARAMETER_NAME_FINAL", "ALTERNATE_MDL", "MDL_STD_UG_M3", "MDL_TYPE", "AQS_NULL_DATA_CODE", "AQS_QUALIFIER_01", "AQS_QUALIFIER_02",
                             "AQS_QUALIFIER_03", "AQS_QUALIFIER_04", "AQS_QUALIFIER_05", "AQS_QUALIFIER_06", "AQS_QUALIFIER_07", "AQS_QUALIFIER_08", "AQS_QUALIFIER_09",
                             "AQS_QUALIFIER_10", "AQS_METHOD_CODE", "SAMPLE_COLLECTION_DESC", "SAMPLE_ANALYSIS_DESC", "SAMPLE_VALUE_FLAG", "BELOW_MDL_FLAG", 
                             "CENSUS_TRACT_ID", "MONITOR_LATITUDE", "MONITOR_LONGITUDE", "PRIORITY_TRENDS")
  air_pollution <- mutate(air_pollution, SAMPLE_DATE = as.Date(SAMPLE_DATE, format = "%m/%d/%Y")) %>% filter(SAMPLE_ANALYSIS_DESC != "None")
  return(air_pollution)
}

server <- function(input, output, session) 
{
  ################
  #Initialization#
  ################
  
  air_pollution <- NULL
  pollutants <- NULL
  CAMAP <- map_data("county") %>% filter(region == "california")
  counties <- distinct(CAMAP, subregion)
  countyHash <- hash()
  countyHash <- get_county_verts(CAMAP, counties$subregion)
  if(is.null(air_pollution))
  {
    progress <- Progress$new(session)
    progress$set(value = 0, message = 'Loading large dataset...')
    progress$inc(amount = 0.2, message = 'Loading large dataset...')
    air_pollution <- initialize_load_data(air_pollution)
    progress$inc(amount = 0.7, message = 'Making parameters...')
    air_pollution <- air_pollution %>% mutate(COUNTY = NA, AMA_SITE_CODE = as.character(AMA_SITE_CODE))
    #print(head(air_pollution))
    default_locs <- distinct(air_pollution, MONITOR_LONGITUDE, MONITOR_LATITUDE)
    pollutants <- distinct(air_pollution, AQS_PARAMETER_NAME)
    programs <- distinct(air_pollution, PROGRAM)
    analysis_meth <- distinct(air_pollution, SAMPLE_ANALYSIS_DESC)
    updateSelectizeInput(session, "PollutantSearch", choices = as.list(pollutants[,1]), server = TRUE)
    updateSelectizeInput(session, "ProgramSearch", choices = as.list(programs[,1]), server = TRUE)
    updateSelectizeInput(session, "AnalysisSearch", choices = as.list(analysis_meth[,1]), server = TRUE)
    updateSliderInput(session, "sideYearRange",  min = min(air_pollution$YEAR), max = max(air_pollution$YEAR), value = c(min(air_pollution$YEAR), max(air_pollution$YEAR)), step = 1)
    progress$inc(amount = 1, message = 'Complete!')
    progress$close()
  }
  
  active_filterDF <- reactiveVal()
  active_filterDF(air_pollution)
  
  locations <- reactiveVal()
  locations(default_locs)
  
  locations_react <- reactive({
    if(input$ShowSelectCheckBox)
    {
      if(is.null(SelectedSitesDF()))
      {
        locations()[1,] %>% filter(1 == 0)
      }
      else
      {
        distinct(SelectedSitesDF(), MONITOR_LONGITUDE, MONITOR_LATITUDE)
      }
    }
    else
    {
      locations()
    } 
  })
  
  default_map_limx <- c(min(CAMAP$long), max(CAMAP$long))
  default_map_limy <- c(min(CAMAP$lat), max(CAMAP$lat))
  
  map_limx <- reactiveVal()
  map_limy <- reactiveVal()
  
  map_limx(default_map_limx)
  map_limy(default_map_limy)
  
  selected_county <- reactiveVal()
  
  SelectedSitesDF <- reactiveVal()
  SelectedSitesDF(NULL)

  observeEvent(input$NavBar,{
    if(input$NavBar == "DescStats")
    {
      fillDescCalcParam()
    }
    if(input$NavBar == "Visual")
    {
      fillVisualParams()
    }
    if(input$NavBar == "Analytic")
    {
      fillStatsParams()
    }
  })
  
  ######################
  #Selection Data Table#
  ######################
  
  output$SelectedList <-renderDataTable({
    if(is.null(SelectedSitesDF()))
    {
      NULL
    }
    else
    {
      print(head(SelectedSitesDF()))
      DT::datatable(SelectedSitesDF()[,c("AMA_SITE_CODE", "PROGRAM", "YEAR", "SAMPLE_DATE", "SAMPLE_START_TIME", "AQS_PARAMETER_NAME",
                                                "SAMPLE_VALUE_STD", "SAMPLE_COLLECTION_DESC", "SAMPLE_ANALYSIS_DESC", "MONITOR_LATITUDE", "MONITOR_LONGITUDE")],
                    colnames = c("Site Code" = "AMA_SITE_CODE", "Program" = "PROGRAM", "Year" = "YEAR", "Date Sampled" = "SAMPLE_DATE", "Time Sampled" = "SAMPLE_START_TIME",
                                 "Pollutant Name" = "AQS_PARAMETER_NAME", "Concentration (μg/L)" = "SAMPLE_VALUE_STD", "Collection Method" = "SAMPLE_COLLECTION_DESC", 
                                 "Analysis Method" = "SAMPLE_ANALYSIS_DESC", "Latitude" = "MONITOR_LATITUDE", "Longitude" = "MONITOR_LONGITUDE"),
                    rownames = NULL,
      )
    }
    #c("AMA_SITE_CODE", "PROGRAM", "YEAR", "SAMPLE_DATE", "SAMPLE_START_TIME", "AQS_PARAMETER_NAME",
      #"SAMPLE_VALUE_STD", "SAMPLE_COLLECTION_DESC", "SAMPLE_ANALYSIS_DESC", "MONITOR_LATITUDE", "MONITOR_LONGITUDE")
  })
  observeEvent(input$SelectedListRemoveRowsButton, {
    if(!is.null(input$SelectedList_rows_selected))
    {
      print(input$SelectedList_rows_selected)
      SelectedSitesDF(SelectedSitesDF()[-(input$SelectedList_rows_selected),])
    }
  })
  
  ################
  #Filter Options#
  ################
  
  observeEvent(input$PollutantSearch, {
    print(lastUpdate)
    if(lastUpdate == "None")
    {
      #print("UPDATING FROM POLL")
      #print(typeof(isolate(input$PollutantSearch)))
      active_filterDF(air_pollution)
      active_filterDF(inputEventHandlerGG(input$PollutantSearch, "PollutantSearch", "AQS_PARAMETER_NAME", active_filterDF(), session))
      active_filterDF(inputEventHandlerGG(input$ProgramSearch, "ProgramSearch", "PROGRAM", active_filterDF(), session))
      active_filterDF(inputEventHandlerGG(input$AnalysisSearch, "AnalysisSearch", "SAMPLE_ANALYSIS_DESC", active_filterDF(), session))
      last_interacted <<- list(input$PollutantSearch, "AQS_PARAMETER_NAME")
    }
    else if(lastUpdate == "PollutantSearch")
    {
      #print("IGNORED FROM POLL")
      lastUpdate <<- "None"
    }
    #print("END OF FILTER UPDATE POLL")
  })
  
  observeEvent(input$ProgramSearch, {
    if(lastUpdate == "None")
    {
      #print("UPDATING FROM PROG")
      active_filterDF(air_pollution)
      active_filterDF(inputEventHandlerGG(input$ProgramSearch, "ProgramSearch", "PROGRAM", active_filterDF(), session))
      active_filterDF(inputEventHandlerGG(input$PollutantSearch, "PollutantSearch", "AQS_PARAMETER_NAME", active_filterDF(), session))
      active_filterDF(inputEventHandlerGG(input$AnalysisSearch, "AnalysisSearch", "SAMPLE_ANALYSIS_DESC", active_filterDF(), session))
      last_interacted <<- list(input$ProgramSearch, "PROGRAM")
    }
    else if (lastUpdate == "ProgramSearch")
    {
      lastUpdate <<- "None"
      #print("IGNORED FROM PROG")
    }
    #print("END OF FILTER UPDATE PROG")
  })
  
  observeEvent(input$AnalysisSearch, {
    if(lastUpdate == "None")
    {
      #print("UPDATING FROM METH")
      active_filterDF(air_pollution)
      active_filterDF(inputEventHandlerGG(input$AnalysisSearch, "AnalysisSearch", "SAMPLE_ANALYSIS_DESC", active_filterDF(), session))
      active_filterDF(inputEventHandlerGG(input$PollutantSearch, "PollutantSearch", "AQS_PARAMETER_NAME", active_filterDF(), session))
      active_filterDF(inputEventHandlerGG(input$ProgramSearch, "ProgramSearch", "PROGRAM", active_filterDF(), session))
      last_interacted <<- list(input$AnalysisSearch, "SAMPLE_ANALYSIS_DESC")
    }
    else if(lastUpdate == "AnalysisSearch")
    {
      #print("IGNORED FROM METH")
      lastUpdate <<- "None"
    }
    print("END OF FILTER UPDATE METH")
  })
  
  observeEvent(input$FilterButton, {
    tempDF <- active_filterDF()
    active_filterDF(filter(active_filterDF(), YEAR >= input$sideYearRange[1] & YEAR <= input$sideYearRange[2]))
    #print(input$sideYearRange[1])
    #print(head(active_filterDF()))
    locations(distinct(active_filterDF(), MONITOR_LONGITUDE, MONITOR_LATITUDE))
    updateMapControlVals()
    #active_filterDF(tempDF)
  })
  
  observeEvent(input$ResetFilterButton, {
    updateSelectizeInput(session, "PollutantSearch", choices = as.list(pollutants[,1]), server = TRUE)
    updateSelectizeInput(session, "ProgramSearch", choices = as.list(programs[,1]), server = TRUE)
    updateSelectizeInput(session, "AnalysisSearch", choices = as.list(analysis_meth[,1]), server = TRUE)
    updateSliderInput(session, "sideYearRange", min = min(air_pollution$YEAR), max = max(air_pollution$YEAR), value = c(min(air_pollution$YEAR), max(air_pollution$YEAR)))
    locations(default_locs)
    active_filterDF(air_pollution)
  })
  
  ###################
  #Selection Options#
  ###################
  
  observeEvent(input$SelectAllButton, {
    if(!is.null(active_filterDF()))
    {
      tempDF <- active_filterDF()
      active_filterDF(filter(active_filterDF(), YEAR >= input$sideYearRange[1] & YEAR <= input$sideYearRange[2]))
      SelectedSitesDF(select(active_filterDF(), c("AMA_SITE_CODE", "PROGRAM", "YEAR", "SAMPLE_DATE", "SAMPLE_START_TIME", "AQS_PARAMETER_NAME",
                                                  "SAMPLE_VALUE_STD", "SAMPLE_COLLECTION_DESC", "SAMPLE_ANALYSIS_DESC", "MONITOR_LATITUDE", "MONITOR_LONGITUDE")))
      print("Before Descriptive")
      fillDescCalcParam() 
      print("Before Visual")
      fillVisualParams()
      print("Before Statistics")
      fillStatsParams()
      print("In select all?")
      active_filterDF(tempDF)
    }
  })
  
  observeEvent(input$ClearSelectButton, {
    SelectedSitesDF(NULL)
    fillVisualParams()
    fillDescCalcParam()
    fillStatsParams()
  })
  
  ##############
  #Map Controls#
  ##############
  
  updateMapControlVals <- function()
  {
    if(input$ShowSelectCheckBox)
    {
      if(is.null(SelectedSitesDF()))
      {
        myDF <- active_filterDF() %>% filter(1 == 0)
      }
      else
      {
        myDF <- SelectedSitesDF()
      }
    }
    else
    {
      myDF <- active_filterDF()
    }
    choice <- as.list(distinct(myDF, AQS_PARAMETER_NAME)[,1])
    
    selected <- input$PollMapSearch
    
    updateSelectizeInput(session, "PollMapSearch", choices = choice, server = TRUE, selected = selected)
    updateSliderInput(session, "MapYearRange", min = min(myDF$YEAR), 
                      max = max(myDF$YEAR, value = min(myDF$YEAR), step = 1))
  }
  
  observeEvent(input$VisPolCheckBox, {
    if(input$VisPolCheckBox)
    {
      updateMapControlVals()
    }
  })
  
  observeEvent(input$ShowSelectCheckBox,
                 if(input$VisPolCheckBox)
                 {
                   updateMapControlVals()
                 }
               )
  
  
  getvispollimts <- reactive({
    if(input$ShowSelectCheckBox)
    {
      if(is.null(SelectedSitesDF()))
      {
        myDF <- active_filterDF() %>% filter(1 == 0)
      }
      else
      {
        myDF <- SelectedSitesDF()
      }
    }
    else
    {
      myDF <- active_filterDF()
    }
    myPointsDF <- filter(myDF, AQS_PARAMETER_NAME %in% input$PollMapSearch)  %>% group_by(MONITOR_LONGITUDE, MONITOR_LATITUDE, YEAR) %>% summarise(Pollution = mean(SAMPLE_VALUE_STD))
    print(myPointsDF)
    c(min(myPointsDF[,"Pollution"], na.rm = TRUE), max(myPointsDF[,"Pollution"], na.rm = TRUE))
  })
  
  getvispolpoints <- reactive({
    myDF <- NULL
    if(input$ShowSelectCheckBox)
    {
      if(is.null(SelectedSitesDF()))
      {
        myDF <- active_filterDF() %>% filter(1 == 0)
      }
      else
      {
        myDF <- SelectedSitesDF()
      }
    }
    else
    {
      myDF <- active_filterDF()
    }
    #print(myDF)
    myPointsDF <- filter(myDF, AQS_PARAMETER_NAME %in% input$PollMapSearch & YEAR == input$MapYearRange) %>% 
      group_by(MONITOR_LONGITUDE, MONITOR_LATITUDE) %>% summarise(Pollution = mean(SAMPLE_VALUE_STD))
  })
  
  output$pollutionMap <- renderPlot({
    if(input$VisPolCheckBox)
    {
      #print(getvispollimts())
      ggplot() + geom_polygon(data = CAMAP, aes(x = long, y = lat, group = subregion), color = "#798d8f", fill = "#2c3e50") + 
        geom_point(data = getvispolpoints(), aes(x = MONITOR_LONGITUDE, y = MONITOR_LATITUDE, size = Pollution, color = Pollution), alpha = 0.9) + 
        scale_color_viridis_c(option = "rocket", limits = getvispollimts()) + scale_size(limits = getvispollimts(), range = c(2,9)) + coord_fixed(ratio = 1, xlim = map_limx(), ylim = map_limy()) + theme_void() + theme(rect = element_blank())
    }
    else
    {
      ggplot() + geom_polygon(data = CAMAP, aes(x = long, y = lat, group = subregion), color = "#798d8f", fill = "#2c3e50") + 
        geom_point(data = locations_react(), aes(x = MONITOR_LONGITUDE, y = MONITOR_LATITUDE), alpha = 0.9, color = "#3498db") +
        coord_fixed(ratio = 1, xlim = map_limx(), ylim = map_limy()) + theme_void() + theme(rect = element_blank())
    }
  })
  
  output$county_text <- renderText({
    if(!is.null(selected_county()))
    {
      toTitleCase(paste(selected_county(), "County"))
    }
    else
    {
      "No County Selected"
    }
  })
  
  output$select_helpertext <- renderText({
    if(input$select_type == "By Area") 
    {
      "Click and drag to select all sites in area."
    }
    else if(input$select_type == "By County")
    {
      "Click on county to select all sites in region."
    }
    else
    {
      "Click on a site to select it\n"
    }
  })
  
  observeEvent(input$click_Map, {
    if(input$EnableSelectCheckBox)
    {
      if(input$select_type == "By County")
      {
        clickmat <- matrix(c(input$click_Map$x, input$click_Map$y), nrow = 1, ncol = 2)
        county <- get_county_pip(clickmat, counties, countyHash)
        if(county != "NiC")
        {
          county_mat <- CAMAP %>% filter(subregion == county) %>% select(long, lat)
          selected_county(county)
          #location_matrix <- data.matrix(locations())
          siteDF <- filter(locations(), point.in.polygon(locations()$MONITOR_LONGITUDE, locations()$MONITOR_LATITUDE, county_mat$long, county_mat$lat) != 0)
          if(nrow(siteDF) > 0)
          {
            newDF <- select(active_filterDF(), c("AMA_SITE_CODE", "PROGRAM", "YEAR", "SAMPLE_DATE", "SAMPLE_START_TIME", "AQS_PARAMETER_NAME",
                                             "SAMPLE_VALUE_STD", "SAMPLE_COLLECTION_DESC", "SAMPLE_ANALYSIS_DESC", "MONITOR_LATITUDE", 
                                             "MONITOR_LONGITUDE")) %>% filter(MONITOR_LATITUDE == siteDF$MONITOR_LATITUDE & MONITOR_LONGITUDE == siteDF$MONITOR_LONGITUDE)
            if(input$select_action == "Replace")
            {
              SelectedSitesDF(newDF)
            }
            else
            {
              SelectedSitesDF(rbind(SelectedSitesDF(), newDF))
            }
          }
        }
      } 
      else if(input$select_type == "Single Site")
      {
        siteDF <- nearPoints(locations(), input$click_Map, xvar = "MONITOR_LONGITUDE", yvar = "MONITOR_LATITUDE")
        #print(siteDF)
        if(nrow(siteDF) > 0)
        {
          newDF <- select(active_filterDF(), c("AMA_SITE_CODE", "PROGRAM", "YEAR", "SAMPLE_DATE", "SAMPLE_START_TIME", "AQS_PARAMETER_NAME",
                                               "SAMPLE_VALUE_STD", "SAMPLE_COLLECTION_DESC", "SAMPLE_ANALYSIS_DESC", "MONITOR_LATITUDE", 
                                               "MONITOR_LONGITUDE")) %>% filter(MONITOR_LATITUDE == siteDF$MONITOR_LATITUDE & MONITOR_LONGITUDE == siteDF$MONITOR_LONGITUDE)
          #print(newDF)
          if(input$select_action == "Replace")
          {
            SelectedSitesDF(newDF)
          }
          else
          {
            SelectedSitesDF(rbind(SelectedSitesDF(), newDF))
          }
        }
      }
      session$resetBrush("brush_Map")
    }
  })
  
  observeEvent(input$hover_Map, {
    if(!input$EnableSelectCheckBox)
    {
      clickmat <- matrix(c(input$hover_Map$x, input$hover_Map$y), nrow = 1, ncol = 2)
      county <- get_county_pip(clickmat, counties, countyHash)
      if(county != "NiC")
      {
        county_map <- CAMAP %>% filter(subregion == county)
        selected_county(county)
        
      }
      else
      {
        selected_county(NULL)
      }
      session$resetBrush("brush_Map")
    }
  })
  
  observeEvent(input$brush_Map, {
    if(!input$EnableSelectCheckBox)
    {
      map_limx(c(input$brush_Map$xmin, input$brush_Map$xmax))
      map_limy(c(input$brush_Map$ymin, input$brush_Map$ymax))
      selected_county(NULL)
      session$resetBrush("brush_Map")
    }
    else
    {
      if(input$select_type == "By Area")
      {
        siteDF <- brushedPoints(locations(), input$brush_Map, xvar = "MONITOR_LONGITUDE", yvar = "MONITOR_LATITUDE")
        #print(siteDF)
        if(nrow(siteDF) > 0)
        {
          newDF <- select(active_filterDF(), c("AMA_SITE_CODE", "PROGRAM", "YEAR", "SAMPLE_DATE", "SAMPLE_START_TIME", "AQS_PARAMETER_NAME",
                                           "SAMPLE_VALUE_STD", "SAMPLE_COLLECTION_DESC", "SAMPLE_ANALYSIS_DESC", "MONITOR_LATITUDE", 
                                           "MONITOR_LONGITUDE")) %>% filter(MONITOR_LATITUDE == siteDF$MONITOR_LATITUDE & MONITOR_LONGITUDE == siteDF$MONITOR_LONGITUDE)
          #print(newDF)
          if(input$select_action == "Replace")
          {
            SelectedSitesDF(newDF)
          }
          else
          {
            SelectedSitesDF(rbind(SelectedSitesDF(), newDF))
          }
        }
      }
      session$resetBrush("brush_Map")
    }
  })
  
  observeEvent(input$dbclick_Map, {
    if(!input$EnableSelectCheckBox)
    {
      map_limx(default_map_limx)
      map_limy(default_map_limy)
      selected_county(NULL)
    }
  })
  
  ########################
  #Descriptive Statistics#
  ########################
  function_hash <- hash(c("Average", "Median", "Standard Deviation", "Minimum", "Maximum"), c(mean, median, sd, min, max))
  
  fillDescCalcParam <- reactive({
    if(!is.null(SelectedSitesDF()))
    {
      updateSelectizeInput(session, "DescriptivePolSearch", choices = as.list((SelectedSitesDF() %>% distinct(AQS_PARAMETER_NAME))[,"AQS_PARAMETER_NAME"]), server = TRUE)
      print(col_conv[[isolate(input$DescriptiveModSearch)]])
      SelectionDF <- filter(SelectedSitesDF(), AQS_PARAMETER_NAME == input$DescriptivePolSearch) %>% select(!!as.symbol(col_conv[[isolate(input$DescriptiveModSearch)]])) %>% 
        distinct(!!as.symbol(col_conv[[input$DescriptiveModSearch]]))
      updateSelectizeInput(session, "DescriptiveModOptionsSearch", choices = as.list(SelectionDF[,1]), selected = SelectionDF[1,1], server = TRUE)
      
      
      SelectionDF <- SelectedSitesDF() %>% select(!!as.symbol(col_conv[[isolate(input$DescriptiveSumForSelSearch)]])) %>% 
        distinct(!!as.symbol(col_conv[[input$DescriptiveSumForSelSearch]]))
      updateSelectizeInput(session, "DescriptiveSumForOptSelSearch", choices = as.list(SelectionDF[,1]), selected = SelectionDF[1,1], server = TRUE)
    }
    else
    {
      updateSelectizeInput(session, "DescriptivePolSearch", choices = NULL, server = TRUE)
      updateSelectizeInput(session, "DescriptiveSumForOptSelSearch", choices = NULL, server = TRUE)
      updateSelectizeInput(session, "DescriptiveModOptionsSearch", choices = NULL, server = TRUE)
    }
  })
  
  Calc_text <- reactiveVal("Click \"Calculate\" to perform selected calculation.")
  summaryDT <- reactiveVal(NULL)
  
  observeEvent(input$DescriptiveSumButton, {
    if(!is.null(SelectedSitesDF()))
    {
      if(input$DescriptiveSumSelSearch != "" & input$DescriptiveSumForSelSearch != "" & input$DescriptiveSumForSelSearch != "")
      {
        
        SelectionDF <- SelectedSitesDF() %>% select(!!as.symbol(col_conv[[isolate(input$DescriptiveSumForSelSearch)]]), !!as.symbol(col_conv[[isolate(input$DescriptiveSumSelSearch)]])) %>% 
          filter(!!as.symbol(col_conv[[isolate(input$DescriptiveSumForSelSearch)]]) == isolate(input$DescriptiveSumForOptSelSearch)) %>% 
          distinct(!!as.symbol(col_conv[[isolate(input$DescriptiveSumForSelSearch)]]), !!as.symbol(col_conv[[input$DescriptiveSumSelSearch]])) %>%
          rename((!!as.symbol(isolate(input$DescriptiveSumForSelSearch))) := (!!as.symbol(col_conv[[isolate(input$DescriptiveSumForSelSearch)]])), 
                 (!!as.symbol(isolate(input$DescriptiveSumSelSearch))) := (!!as.symbol(col_conv[[isolate(input$DescriptiveSumSelSearch)]])))

        summaryDT(DT::datatable(SelectionDF, rownames = NULL))
      }
      else
      {
        summaryDT(NULL)
      }
    }
      #colnames = c(!!as.symbol(isolate(input$DescriptiveSumForSelSearch)) = !!as.symbol(col_conv[[isolate(input$DescriptiveSumForSelSearch)]]), 
                 #!!as.symbol(isolate(input$DescriptiveSumSelSearch)) = !!as.symbol(col_conv[[isolate(input$DescriptiveSumSelSearch)]]))
  })
  
  observeEvent(input$DescriptiveModSearch, {
    fillDescCalcParam()
  })
  
  observeEvent(input$DescriptivePolSearch, {
    if(!is.null(SelectedSitesDF()))
    {
      SelectionDF <- filter(SelectedSitesDF(), AQS_PARAMETER_NAME == input$DescriptivePolSearch) %>% select(!!as.symbol(col_conv[[isolate(input$DescriptiveModSearch)]])) %>% 
        distinct(!!as.symbol(col_conv[[input$DescriptiveModSearch]]))
      updateSelectizeInput(session, "DescriptiveModOptionsSearch", choices = as.list(SelectionDF[,1]), selected = SelectionDF[1,1], server = TRUE)
    }
  })
  
  observeEvent(input$DescriptiveSumSelSearch, {
    choices <- CategoricalValuesList
    #print("We Here")
    for(c in choices)
    {
      if(grepl(c, input$DescriptiveSumSelSearch))
      {
        choices <- choices[choices != c]
      }
    }
    print(choices)
    updateSelectizeInput(session, "DescriptiveSumForSelSearch", choices = choices, server = TRUE)
  })  
  
  observeEvent(input$DescriptiveSumForSelSearch, {
    if(!is.null(SelectedSitesDF()))
    {
      if(input$DescriptiveSumForSelSearch != "")
      {
        #print("Did I get Here??")
        SelectionDF <- SelectedSitesDF() %>% select(!!as.symbol(col_conv[[isolate(input$DescriptiveSumForSelSearch)]])) %>% 
          distinct(!!as.symbol(col_conv[[input$DescriptiveSumForSelSearch]]))
        #print("Did I get Here Though??")
        updateSelectizeInput(session, "DescriptiveSumForOptSelSearch", choices = as.list(SelectionDF[,1]), selected = SelectionDF[1,1],server = TRUE)
        
      }
    }
  })  
  
  observeEvent(input$DescriptiveCalcButton, {
    if(!is.null(SelectedSitesDF()))
    {
      myDF <- filter(SelectedSitesDF(), AQS_PARAMETER_NAME == input$DescriptivePolSearch) %>% filter(!!as.symbol(col_conv[[isolate(input$DescriptiveModSearch)]]) == input$DescriptiveModOptionsSearch)
      calculation_output <- function_hash[[isolate(input$DescriptiveCalcSearch)]](myDF[,"SAMPLE_VALUE_STD"], na.rm = TRUE)
      Calc_text(paste("The ", input$DescriptiveCalcSearch, " concentration of ", input$DescriptivePolSearch, " for ", input$DescriptiveModSearch, " ", input$DescriptiveModOptionsSearch, "\n = ", calculation_output, "μg/L"))
    }
  })
  
  output$descriptiveTextOutput <- renderText({
    if(is.null(SelectedSitesDF()))
    {
      "No data selected..."
    }
    else
    {
      Calc_text()
    }
  })
  
  output$SummaryTable <- renderDataTable({
    summaryDT()
  })
  
  output$DescModOptText <- renderText({
    paste(input$DescriptiveModSearch, ":")
  })
  
  output$DescSumForOptText <- renderText({
    paste(input$DescriptiveSumForSelSearch, ":")
  })
  
  ###############
  #Visualization#
  ###############

  VisualTabGGPlot <- reactiveVal()
  
  use_all_pol <- reactive({
    if(!is.null(SelectedSitesDF()))
    {
      if(input$VisualPlotSearch == "Box Plot" && input$VisualPlotSearch != "")
      {
        if(input$VisualBPDisc1 == "Pollutant" | input$VisualBPDisc2 == "Pollutant")
        {
          return(TRUE)
        }
        else
        {
          return(FALSE)
        }
      }
      if(input$VisualPlotSearch == "Histogram")
      {
        if(input$VisualHGDisc1 == "Pollutant" | input$VisualHGDisc2 == "Pollutant")
        {
          return(TRUE)
        }
        else
        {
          return(FALSE)
        }
      }
    }
  })
  
  fillVisualParams <- function()
  {
    if(!is.null(SelectedSitesDF()))
    {
      updateSelectizeInput(session, "VisualBPPol", choices = as.list(SelectedSitesDF()[,"AQS_PARAMETER_NAME"]), server = TRUE)
      updateSelectizeInput(session, "VisualHGPol", choices = as.list(SelectedSitesDF()[,"AQS_PARAMETER_NAME"]), server = TRUE)
      updateSelectizeInput(session, "VisualLGPol", choices = as.list(SelectedSitesDF()[,"AQS_PARAMETER_NAME"]), server = TRUE)
    }
    else
    {
      updateSelectizeInput(session, "VisualBPPol", choices = NULL, server = TRUE)
      updateSelectizeInput(session, "VisualHGPol", choices = NULL, server = TRUE)
      updateSelectizeInput(session, "VisualLGPol", choices = NULL, server = TRUE)
    }
  }
  
  make_boxplot_obj <- function(myDF)
  {
    if(input$VisualBPDisc2 != "")
    {
      plot_out <- ggplot(data = myDF, aes(x = !!as.symbol(col_conv[[isolate(input$VisualBPDisc1)]]), 
                                                       y = SAMPLE_VALUE_STD, 
                                                       color = !!as.symbol(col_conv[[isolate(input$VisualBPDisc2)]])))
      plot_out <- plot_out + geom_boxplot()
      plot_out <- plot_out + labs(x = isolate(input$VisualBPDisc1), y = "Concentration (μg/L)", color = input$VisualBPDisc2)
    }
    else
    {
      plot_out <- ggplot(data = myDF, aes(x = !!as.symbol(col_conv[[isolate(input$VisualBPDisc1)]]), 
                                                       y = SAMPLE_VALUE_STD))
      plot_out <- plot_out + geom_boxplot()
      plot_out <- plot_out + labs(x = isolate(input$VisualBPDisc1), y = "Concentration (μg/L)")
    }
    return(plot_out)
  }
  
  make_histogram_obj <- function(myDF)
  {
    if(input$VisualHGDisc2 != "")
    {
      plot_out <- ggplot(data = myDF, aes(x = !!as.symbol(col_conv[[isolate(input$VisualHGDisc1)]]), 
                                          y = SAMPLE_VALUE_STD, 
                                          fill = !!as.symbol(col_conv[[isolate(input$VisualHGDisc2)]])))
      plot_out <- plot_out + geom_col(width = 0.5)
      plot_out <- plot_out + labs(x = isolate(input$VisualHGDisc1), y = "Concentration (μg/L)", fill = input$VisualHGDisc2)
    }
    else
    {
      plot_out <- ggplot(data = myDF, aes(x = !!as.symbol(col_conv[[isolate(input$VisualHGDisc1)]]), 
                                          y = SAMPLE_VALUE_STD))
      plot_out <- plot_out + geom_col(width = 0.5)
      plot_out <- plot_out + labs(x = isolate(input$VisualHGDisc1), y = "Concentration (μg/L)")
    }
    return(plot_out)
  }
  
  observeEvent(input$VisualPlotMakeButton, {
    if(!is.null(SelectedSitesDF()))
    {
      if(input$VisualPlotSearch == "Box Plot")
      {
        if(use_all_pol())
        {
          plot_out <- make_boxplot_obj(SelectedSitesDF())
        }
        else
        {
          myDF <- SelectedSitesDF() %>% filter(AQS_PARAMETER_NAME == input$VisualBPPol)
          plot_out <- make_boxplot_obj(myDF)
        }
      }
      if(input$VisualPlotSearch == "Histogram")
      {
        if(use_all_pol())
        {
          plot_out <- make_histogram_obj(SelectedSitesDF())
        }
        else
        {
          myDF <- SelectedSitesDF() %>% filter(AQS_PARAMETER_NAME == input$VisualHGPol)
          plot_out <- make_histogram_obj(myDF)
        }
      }
      if(input$VisualPlotSearch == "Line Graph")
      {
        if(isolate(input$VisualLGPX) != "")
        {
          myDF <- SelectedSitesDF() %>% filter(AQS_PARAMETER_NAME %in% isolate(input$VisualLGPol))
          plot_out <- ggplot(data = myDF, mapping = aes(x = !!as.symbol(col_conv[[isolate(input$VisualLGPX)]]), y = SAMPLE_VALUE_STD,  color = AQS_PARAMETER_NAME)) + 
            geom_line() + labs(x = isolate(input$VisualLGPX), y = "Concentration (μg/L)", color = "Pollutant")
        }
        else
        {
          plot_out <- ggplot()
        }
      }
      plot_out <- plot_out + theme_minimal()
      VisualTabGGPlot(plot_out)
    }
  })
  
  output$VisualTabPlot <- renderPlot({
    VisualTabGGPlot()
  })
  ############
  #Statistics#
  ############
  fillStatsParams <- function()
  {
    print("Made it here")
    if(!is.null(SelectedSitesDF()))
    {
      print("Now Here")
      updateSelectizeInput(session, "StatsCatA", choices = CategoricalValuesList, selected = "Site", server = TRUE)
      updateSelectizeInput(session, "StatsCatB", choices = CategoricalValuesListPureCatNoP, selected = "Site", server = TRUE)
      print("Now Here2")
      statscatDF <- SelectedSitesDF() %>% distinct(!!as.symbol(col_conv[["Site"]]))
      updateSelectizeInput(session, "StatsCatAOpt", choices = as.list(statscatDF[,1]), server = TRUE)
      print("Now Here3")
      statscatDF <- SelectedSitesDF() %>% distinct(!!as.symbol(col_conv[["Site"]]))
      updateSelectizeInput(session, "StatsCatBOpt", choices = as.list(statscatDF[,1]), server = TRUE)
      print("Now Out")
    }
    else
    {
      updateSelectizeInput(session, "StatsCatP", choices = NULL, server = TRUE)
      updateSelectizeInput(session, "StatsCatP2", choices = NULL, server = TRUE)
      updateSelectizeInput(session, "StatsCatAOpt", choices = NULL, server = TRUE)
      updateSelectizeInput(session, "StatsCatBOpt", choices = NULL, server = TRUE)
      updateSelectizeInput(session, "StatsCatA", choices = NULL, server = TRUE)
      updateSelectizeInput(session, "StatsCatB", choices = NULL, server = TRUE)
    }
  }

  StatsTextOut <- reactiveVal("Press \"Calculate\" to perform test.")
  
  observeEvent(input$StatsCatA, {
    if(!is.null(SelectedSitesDF()))
    {
      if(input$StatsCatA != "" & !is.null(input$StatsCatA))
      {
        statscatDF <- SelectedSitesDF() %>% distinct(!!as.symbol(col_conv[[isolate(input$StatsCatA)]]))
        updateSelectizeInput(session, "StatsCatAOpt", choices = as.list(statscatDF[,1]), server = TRUE)
        if(input$StatsPickTest == "Two Sample t-Test")
        {
          if(input$StatsCatA == "Pollutant")
          {
            updateSelectizeInput(session, "StatsCatB", choices = c("Pollutant"), selected = "Pollutant", server = TRUE)
          }
          else
          {
            updateSelectizeInput(session, "StatsCatB", choices = CategoricalValuesListPureCatNoP , server = TRUE)
            #print("Tail CatA")
          }
        }
      }
    }
  })
  
  updateCatP2 <- function()
  {
    #print("in catP2")
    if(input$StatsCatBOpt != "" & !is.null(input$StatsCatBOpt) & input$StatsCatAOpt != "" & !is.null(input$StatsCatAOpt) & 
       input$StatsCatA != "" & !is.null(input$StatsCatA) & input$StatsCatB != "" & !is.null(input$StatsCatB))
    {
      statscatDF <- SelectedSitesDF() %>% filter(!!as.symbol(col_conv[[isolate(input$StatsCatA)]]) == isolate(input$StatsCatAOpt)) %>%
        filter(!!as.symbol(col_conv[[isolate(input$StatsCatB)]]) == isolate(input$StatsCatBOpt))
      updateSelectizeInput(session, "StatsCatP2", choices = as.list(statscatDF[,"AQS_PARAMETER_NAME"]), server = TRUE)
      #print("Tail CatP2")
    }
  }
  
  observeEvent(input$StatsCatAOpt, {
    if(!is.null(SelectedSitesDF()))
    {
      if(input$StatsCatAOpt != "" & !is.null(input$StatsCatAOpt))
      {
        if(input$StatsPickTest == "One Sample t-Test")
        {
          statscatDF <- SelectedSitesDF() %>% filter(!!as.symbol(col_conv[[isolate(input$StatsCatA)]]) == isolate(input$StatsCatAOpt))
          updateSelectizeInput(session, "StatsCatP", choices = as.list(statscatDF[,"AQS_PARAMETER_NAME"]), server = TRUE)
        }
        else
        {
          updateCatP2()
        }
      }
    }
  })
  
  observeEvent(input$StatsCatBOpt, {
   # print("cat Bopt ")
      if(!is.null(SelectedSitesDF()))
      {
        updateCatP2()
      }
    #print("outofCatBopt")
    })
  
  
  observeEvent(input$StatsCatB, {
    #print("in CatB")
    if(!is.null(SelectedSitesDF()))
    {
      if(input$StatsCatB != "" & !is.null(input$StatsCatB))
      {
        statscatDF <- SelectedSitesDF() %>% distinct(!!as.symbol(col_conv[[isolate(input$StatsCatB)]]))
        updateSelectizeInput(session, "StatsCatBOpt", choices = as.list(statscatDF[,1]), server = TRUE) 
      }
    }
    #print("End of CatB")
  })
  
  observeEvent(input$StatsPickTest, {
    if(!is.null(SelectedSitesDF()))
    {
      if(input$StatsPickTest == "One Sample t-Test")
      {
        updateSelectizeInput(session, "StatsCatA", choices = CategoricalValuesListNonP, server = TRUE)
      }
      else
      {
        updateSelectizeInput(session, "StatsCatA", choices = CategoricalValuesList, server = TRUE)
        updateSelectizeInput(session, "StatsCatB", choices = CategoricalValuesListPureCatNoP, server = TRUE)
      }
    }
  })
  
  t_test_one_safe <- function(df, m) 
  {
    obj<-try(t.test(df, mu = m, na.rm=TRUE), silent=TRUE)
    if (is(obj, "try-error"))
    { 
      print(obj)
      return(NA) 
    }
    else
    {
       return(obj)
    }
  }
  
  t_test_two_safe <- function(df, ydf) 
  {
    obj<-try(t.test(df, ydf, na.rm=TRUE), silent=TRUE)
    if (is(obj, "try-error"))
    { 
      print(obj)
      return(NA) 
    }
    else
    {
      return(obj)
    }
  }
  
  observeEvent(input$StatsCalcButton, {
    if(!is.null(SelectedSitesDF()))
    {
      if(input$StatsPickTest == "One Sample t-Test")
      {
        myDF <- SelectedSitesDF() %>% filter(!!as.symbol(col_conv[[isolate(input$StatsCatA)]]) == isolate(input$StatsCatAOpt)) %>% filter(AQS_PARAMETER_NAME == isolate(input$StatsCatP))
        print(head(myDF))
        
        tDF <- SelectedSitesDF() %>% filter(AQS_PARAMETER_NAME == isolate(input$StatsCatP))
        #print(tDF$SAMPLE_VALUE_STD
        mu <- mean(tDF$SAMPLE_VALUE_STD, na.rm = TRUE)
        print(length(tDF$SAMPLE_VALUE_STD))
        
        t.result <- t_test_one_safe(myDF$SAMPLE_VALUE_STD, mu)
        
        if(is.na(t.result))
        {
          StatsTextOut(geterrmessage())
          return()
        }
        
        print(t.result)
        
        if(t.result[3] < 0.05)
        {
          result_string <- paste("Subset mean concentration of ", isolate(input$StatsCatP), "(", t.result[[5]],")\n is significantly different from dataset mean concentration (", mu, ")")
        }
        else
        {
          result_string <- paste("Subset mean concentration of ", isolate(input$StatsCatP), "(", t.result[[5]],")\n is not significantly different from dataset mean concentration (", mu, ")")
        }
        StatsTextOut(paste("One Sample t-Test (95% Confidence):\n", "t = ", t.result[[1]], "df = ", t.result[[2]], "p = ", t.result[3], "\n", result_string))
      }
      else
      {
        #browser()
        myDFA <- NULL
        myDFB <- NULL
        result_string <- NULL
        if(isolate(input$StatsCatA) != "Pollutant")
        {
          updateCatP2()
          
          myCatA <- col_conv[[isolate(input$StatsCatA)]]
          myCatAOpt <- isolate(input$StatsCatAOpt)
          myCatB <- col_conv[[isolate(input$StatsCatB)]]
          myCatBOpt <- isolate(input$StatsCatBOpt)
          myCatP <- isolate(input$StatsCatP2)
          
          #print(myCatA)
          
          myDFA <- SelectedSitesDF() %>% filter(!!as.symbol(myCatA) == isolate(myCatAOpt))
          print(myCatP)
          myDFA <- myDFA %>% filter(AQS_PARAMETER_NAME == myCatP)
          print(head(myDFA))
          myDFB <- SelectedSitesDF() %>% filter(!!as.symbol(myCatB) == isolate(myCatBOpt)) %>% filter(AQS_PARAMETER_NAME == myCatP)
          
          result_string_moduleA <- paste(input$StatsCatP2, "\nfor", isolate(input$StatsCatA), ":", isolate(input$StatsCatAOpt))
          result_string_moduleB <- paste(input$StatsCatP2, "\nfor", isolate(input$StatsCatB), ":", isolate(input$StatsCatBOpt))
        }
        else
        {
          myDFA <- SelectedSitesDF() %>% filter(!!as.symbol(col_conv[[isolate(input$StatsCatA)]]) == isolate(input$StatsCatAOpt))
          myDFB <- SelectedSitesDF() %>% filter(!!as.symbol(col_conv[[isolate(input$StatsCatB)]]) == isolate(input$StatsCatBOpt))
          
          result_string_moduleA <- paste(input$StatsCatP2)
          result_string_moduleB <- paste(input$StatsCatP2)
        }
        t.result <- t_test_two_safe(myDFA$SAMPLE_VALUE_STD, myDFB$SAMPLE_VALUE_STD)
        if(is.na(t.result))
        {
          StatsTextOut(geterrmessage())
          return()
        }
        if(t.result[3] < 0.05)
        {
          result_string <- paste("Subset mean concentration of ", result_string_moduleA, "\nis significantly different to the mean concentration of ", result_string_moduleB, "(", t.result[[5]]," difference in means)\n")
        }
        else
        {
          result_string <- paste("Subset mean concentration of ", result_string_moduleA, "\nis not significantly different to the mean concentration of ", result_string_moduleB, "\n(", t.result[[5]]," difference in means)\n")
        }
        #print("Two Sample b4 textout")
        final_string <- paste("Two Sample t-Test (95% Confidence):\n", "t=", t.result[[1]], "df=", t.result[[2]], "p=", t.result[3], "\n", result_string)
        #print(final_string)
        StatsTextOut(final_string[1])
      }
    }
  })
  
  output$StatsTextOutput <- renderText({
    if(is.null(SelectedSitesDF()))
    {
      "No data selected..."
    }
    else
    {
      #print("called twice?")
      StatsTextOut()
    }
  })
}
#thematic_shiny()
#shinyApp(ui = ui, server = server)