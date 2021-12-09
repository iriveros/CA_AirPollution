# California Air Pollution App

This is an interface for exploring an EPA Ambient Monitoring Database, specifically for the state of California with over 1.6 million datapoints from 1990 to 2018. Using this RShiny App you can get a closer, more detailed look at not only air pollution trends, but the scientific methods used to gather the data in an intuitive GUI, though visualization, descriptive and some statistical analysis. This app is also well modularized, and could easily be applied to other, similar, data sets with little effort.

## Running this app.
Using the RShiny Library for R, one can install this app and its dataset directly using:
```R
install.packages('shiny')
library(shiny)
shiny::runGitHub("CA_AirPollution","iriveros")
```
Additionally, you can access the app through shiny.io here:

https://iriveros2017.shinyapps.io/rstudio/?_ga=2.87315352.1436549406.1639073109-1600907200.1639073109

However, the shiny.io version only has a very small portion of the data set available to it due to memory constraints. For full use of the app it is recommend you install using the command listed in an R console.

## Description
To explore the database in reasonable chunks, this app has a few features dedicated specifically to selecting data. On the left hand side of the app, you will find an EPA site filter tool. This is the first level of control over dividing up the dataset for further study. Here you can select to filter for specific pollutants, analysis methods, or EPA programs to narrow down your search using 3 drop down menus and a date range slider. You may notice that the number of items available for you to filter by drops as you select more items, this is because most of the drop down menus in this app will dynamically update to exclude any values which will yeild no results, to save time and guarantee at least one result after filtering. By default, if any drop down menu is left blank, it signifies to not apply a filter for that category, hence, leaving all fields blank will leave the entire dataset intact. One you have selected values to filter, you must hit "Apply Filter", and then "Reset", to turn all filters off again.

Beneath the filter menus are a few tools for selecting the data upon filtering it. The first button does as it says, all values which have been filtered will be added to the selection. This will be reflected in the right hand side datatable being populated with the selected values. Besides that is a "Clear Selection" button which will remove all items from the seleciton, again reflected in the data table on the right. Below these are a few tools to select specific values using the interactive map in the center of the screen. This map by default will display the sites in the active filters. When the "Enable map selection tools" check box is left unticked, you are able to zoom into the map by clicking and dragging, which will briefly highlight a region which will be the new frame of the map, double clicking on the map will reset the view back to default from any position. Upon ticking the map selection tools box, zooming on the map will be disabled, and the map will be used for selecting data. You will see three options for selecting: "By area" lets you draw a square to quickly select all the points in that area. "By County" allows you to click on a county on a map and select all the sites within the region. "Single Site" allows you to click on individual sites to add to the selection. Any active filters still apply with this tool, so if you have Lead selected as your pollutant filter, any sites selected on the map will only add their data points containing lead to the selection. To the right of the map is the data table containing all of yoour selected points. Here you may click on rows to highlight them, and then remove them using the "Remove Highlighted Rows" Button. Below the table and the map are a few visual tools to visualize pollution by year and pollutant, as the ability to show only selected values on the map. 

All data for the following sections comes from the selections made on the first tab.

Under the "Descriptive Statistics" Tab you will find a summarization table and a ways of identifying some statistics on pollutants. On the left hand side you can summarize different discrete and categorical variables to get extra information such as how long certain programs ran for, or what instruments were used to detect the concentration of a specific pollutand. On the right hand side you can calculate the mean, median, max, min and standard devation for a pollutant given a c ouple of available fitlers. 
On the next tab are a few options for graphing data, namely a box plot, bar chart, and line plot. On the box plots you can select a categorical variable for the X-axis, another categorical variable for color grouping, and a pollutant whose concentration based on the other parameters will compose the Y axis. The bar chart is similar, but allows you to also add discrete variables such as the date to the X-axis. The line plot allows you to analyze pollutant concentrations as is relates to time, date and year, as well as plot multiple lines on the chart for comparison. 
On the final tab is a method for performing a one sample and two sample t-test on the data by creating subsets of the selected data based on a few variables and comparing it either to other subsets, or the entire dataset.

## Sources

Data originally sourced from here: https://www.epa.gov/outdoor-air-quality-data
