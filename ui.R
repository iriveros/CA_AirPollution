source("global.R")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("California Air Pollution"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("EPA Site Filter"),
      p(
        "Filter specific EPA sites displayed on the map and data available for selection. You must click \"Reset\" before applying new filter. Data will not be filtered by a category left blank."
      ),
      selectizeInput(
        inputId = "PollutantSearch",
        h4("Select Pollutants"),
        choices = NULL,
        multiple = TRUE
      ),
      p(
        "(PM10: Particulate Matter < 10μm, tsp: Total Suspended Particulate)"
      ),
      selectizeInput(
        inputId = "ProgramSearch",
        h4("Select Programs"),
        choices = NULL,
        multiple = TRUE
      ),
      selectizeInput(
        inputId = "AnalysisSearch",
        h4("Select Analysis Methods"),
        choices = NULL,
        multiple = TRUE
      ),
      sliderInput(
        inputId = "sideYearRange",
        label = h4("Set Year Range"),
        min = 0,
        max = 1,
        value = c(0, 1),
        ticks = FALSE
      ),
      fillRow(
        actionButton("FilterButton", "Apply Filter"),
        actionButton("ResetFilterButton", "Reset"),
        height = 50,
        width = 250
      ),
      hr(),
      h3("Data Selection Tools"),
      p(
        "Select all data in filter or select specific sites using the interactive map."
      ),
      fillRow(
        actionButton("SelectAllButton", "Select All From Filter"),
        actionButton("ClearSelectButton", "Clear Selection"),
        height = 50,
        width = 370
      ),
      checkboxInput(inputId = "EnableSelectCheckBox", label = "Enable map selection tools? (Disables zooming/hover on map)"),
      conditionalPanel(condition = "input.EnableSelectCheckBox",
                       fluidRow(
                         column(
                           5,
                           radioButtons(
                             inputId = "select_type",
                             label = "Select:",
                             choices = c("By Area", "By County", "Single Site"),
                             selected = "By Area"
                           ),
                           textOutput(outputId = "select_helpertext"),
                         ),
                         column(
                           3,
                           radioButtons(
                             inputId = "select_action",
                             label = "Action:",
                             choices = c("Replace", "Aggregate"),
                             selected = "Replace"
                           ),
                         ),
                       ),)
    ),
    mainPanel(
      width = 9,
      navbarPage(
        "Tools",
        id = "NavBar",
        tabPanel(
          "Data Selection",
          fillRow(
            h3("Interactive California EPA Site Map"),
            fillRow(
              h3("Selected Data"),
              actionButton(inputId = "SelectedListRemoveRowsButton", label = "Remove Highlighted Rows"),
              width = 1000
            ),
            height = 50
          ),
          splitLayout(
            plotOutput(
              outputId = "pollutionMap",
              click = "click_Map",
              brush = brushOpts(
                id = "brush_Map",
                delay = 1000,
                delayType = "debounce"
              ),
              dblclick = "dbclick_Map",
              hover = "hover_Map",
              height = "450px",
              width = "450px"
            ) %>% withSpinner(),
            dataTableOutput(outputId = "SelectedList") %>% withSpinner(),
          ),
          fluidRow(column(
            3,
            p(strong("Hover over map to see county name:")),
            htmlOutput(outputId = "county_text"),
          ),
          column(3,
                 p(
                   strong("Click and drag to zoom into area. Double click to reset view.")
                 ),),),
          fluidRow(hr(),
                   fluidRow(
                     column(
                       3,
                       h3("Map Contols"),
                       checkboxInput(inputId = "ShowSelectCheckBox", label = "Only show selected"),
                       checkboxInput(inputId = "VisPolCheckBox", label = "Visualize pollution")
                     ),
                     conditionalPanel(
                       condition = "input.VisPolCheckBox",
                       column(
                         4,
                         selectizeInput(
                           inputId = "PollMapSearch",
                           h4("Select Pollutant"),
                           choices = NULL,
                           multiple = TRUE
                         ),
                         sliderInput(
                           inputId = "MapYearRange",
                           label = h4("Set Date"),
                           min = 0,
                           max = 1,
                           value = 0,
                           step = 1
                         )
                       ),
                       column(
                         5,
                         p(
                           "All concentrations displayed are in μg/L. Values are calculated as yearly averages of all selected pollutants. Click \"Apply Filter\" to update map pollution visuals with filtered values. If points are missing, it is because those site reported an average concentration of 0 for the pollutant for that year."
                         )
                       )
                     ),
                   ))
        ),
        tabPanel(
          "Descriptive Statistics",
          value = "DescStats",
          splitLayout(
            dataTableOutput(outputId = "SummaryTable") %>% withSpinner(),
            fillCol(
              h3("Calculation Output:"),
              h4(strong(
                verbatimTextOutput("descriptiveTextOutput")
              )),
              flex = c(1, 7),
              height = 400
            ),
          ),
          
          fluidRow(
            hr(),
            fluidRow(column(6,
                            h3(
                              "Summarize Selected Data"
                            ),),
                     
                     column(4,
                            h3(
                              "Describe Selected Data"
                            ),),),
            fluidRow(
              column(
                width = 6,
                selectizeInput(
                  inputId = "DescriptiveSumSelSearch",
                  h5("Summarize:"),
                  choices = CategoricalValuesListPlural,
                  multiple = FALSE
                ),
                fillRow(
                  width = 680,
                  height = 95,
                  selectizeInput(
                    inputId = "DescriptiveSumForSelSearch",
                    h5("For:"),
                    choices = NULL,
                    multiple = FALSE
                  ),
                  selectizeInput(
                    inputId = "DescriptiveSumForOptSelSearch",
                    h5(textOutput(outputId = "DescSumForOptText")),
                    choices = NULL,
                    multiple = FALSE
                  )
                ),
                actionButton(inputId = "DescriptiveSumButton", label = "Summarize")
              ),
              column(
                3,
                selectizeInput(
                  inputId = "DescriptivePolSearch",
                  h5("Select Pollutant to study:"),
                  choices = NULL,
                  multiple = FALSE
                ),
                selectizeInput(
                  inputId = "DescriptiveModSearch",
                  h5("Based on:"),
                  choices = CategoricalValuesListNonP,
                  multiple = FALSE
                ),
                actionButton(inputId = "DescriptiveCalcButton", label = "Calculate")
              ),
              column(
                3,
                
                selectizeInput(
                  inputId = "DescriptiveCalcSearch",
                  h5("Calculate:"),
                  choices = c("Average", "Median", "Standard Deviation", "Minimum", "Maximum"),
                  multiple = FALSE
                ),
                selectizeInput(
                  inputId = "DescriptiveModOptionsSearch",
                  h5(textOutput(outputId = "DescModOptText")),
                  choices = NULL,
                  multiple = FALSE
                ),
              ),
              
            )
          )
        ),
        tabPanel(
          "Visualization",
          value = "Visual",
          plotOutput(outputId = "VisualTabPlot", height = "550px"),
          hr(),
          column(
            3,
            selectizeInput(
              inputId = "VisualPlotSearch",
              h4("Choose Plot Type:"),
              choices = c("Box Plot", "Histogram", "Line Graph"),
              multiple = FALSE
            ),
            actionButton(inputId = "VisualPlotMakeButton", label = "Generate Plot"),
            p(
              strong(
                "NOTE: Large data selections with too many variables will make messy plots."
              )
            ),
            p(
              "Outliers, or any other point you wish, can be removed using the \"Remove Highlighted Rows\" Button on the \"Data Selection\" tab"
            )
          ),
          
          conditionalPanel(
            "input.VisualPlotSearch == 'Box Plot'",
            column(
              3,
              selectInput(
                inputId = "VisualBPDisc1",
                h4("Choose Main Categorical Variable:"),
                choices =  CategoricalValuesListPureCat,
                multiple = FALSE
              ),
              selectizeInput(
                inputId = "VisualBPDisc2",
                h4("Choose Secondary Categorical Variable:"),
                choices =  CategoricalValuesListPureCat,
                multiple = FALSE
              ),
              h6("(Optional)")
            ),
            column(
              3,
              conditionalPanel(
                "input.VisualBPDisc1 != 'Pollutant' && input.VisualBPDisc2 != 'Pollutant'",
                selectizeInput(
                  inputId = "VisualBPPol",
                  h4("Choose Pollutant:"),
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              p("Pollutant concentration is the continuous variable.")
            )
          ),
          conditionalPanel(
            "input.VisualPlotSearch == 'Histogram'",
            column(
              3,
              selectInput(
                inputId = "VisualHGDisc1",
                h4("Choose Discrete Variable:"),
                choices = c(CategoricalValuesList, "Date"),
                multiple = FALSE
              ),
              selectInput(
                inputId = "VisualHGDisc2",
                h4("Choose Categorical Variable:"),
                choices = CategoricalValuesListPureCat,
                multiple = FALSE
              )
            ),
            column(
              3,
              conditionalPanel(
                "input.VisualHGDisc1 != 'Pollutant' && input.VisualHGDisc2 != 'Pollutant'",
                selectizeInput(
                  inputId = "VisualHGPol",
                  h4("Choose Pollutant:"),
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              p("Pollutant concentration is the continuous variable.")
            )
            
          ),
          conditionalPanel(
            "input.VisualPlotSearch == 'Line Graph'",
            column(
              3,
              selectizeInput(
                inputId = "VisualLGPol",
                h4("Choose Pollutants:"),
                choices = NULL,
                multiple = TRUE
              ),
              selectInput(
                inputId = "VisualLGPX",
                h4("Choose X axis variable:"),
                choices = c("Time", "Date", "Year"),
                multiple = FALSE
              )
            ),
          ),
        ),
        tabPanel(
          "Statistical Analysis",
          value = "Analytic",
          fluidRow(h3("t-Tests")),
          column(
            4,
            selectInput(
              inputId = "StatsPickTest",
              h4("Select Test"),
              c("One Sample t-Test", "Two Sample t-Test"),
              selected = "Correlation",
              multiple = FALSE
            ),
            h4("Choose Categories:"),
            fillRow(
              selectizeInput(
                inputId = "StatsCatA",
                NULL,
                choices = NULL,
                multiple = FALSE
              ),
              selectizeInput(
                inputId = "StatsCatAOpt",
                NULL,
                choices = NULL,
                multiple = FALSE
              ),
              width = "100%",
              height = 50
            ),
            conditionalPanel(
              "input.StatsPickTest == 'One Sample t-Test'",
              selectInput(
                inputId = "StatsCatP",
                h5("Choose pollutant to test against :"),
                choices = NULL,
                multiple = FALSE
              ),
              p(
                "(Test if mean of subset is different to mean of entire selected dataset)"
              )
            ),
            conditionalPanel(
              "input.StatsPickTest == 'Two Sample t-Test'",
              fillRow(
                selectizeInput(
                  inputId = "StatsCatB",
                  NULL,
                  choices = NULL,
                  multiple = FALSE
                ),
                selectizeInput(
                  inputId = "StatsCatBOpt",
                  NULL,
                  choices = NULL,
                  multiple = FALSE
                ),
                width = "100%",
                height = 50
              ),
              p("(Test if mean of subset is different to mean of another subset)"),
              conditionalPanel(
                "input.StatsCatB != 'Pollutant' && input.StatsCatA != 'Pollutant'",
                selectizeInput(
                  inputId = "StatsCatP2",
                  h4("Choose Pollutant to compare:"),
                  choices = NULL,
                  multiple = FALSE
                )
              ),
            ),
            actionButton(inputId = "StatsCalcButton", label = "Calculate")
          ),
          column(8,
                 h4(strong(
                   verbatimTextOutput("StatsTextOutput")
                 )))
        ),
      )
    )
  )
)