# NASA Genelab Environmental Data Visualization App
*R Shiny visualization tool for spaceflight environmental data.*

## About the Project
NASA GeneLab creates an open-access repository of data collected from the International Space Station with information relevant to how life adapts in space. This data includes omics, environmental data, data collected during experiments conducted on the ISS, and more.
> “Any and all data regarding the conditions under which an experiment is conducted may have bearing on how the data produced during the experiment are interpreted; these conditions, explicitly documented or not, are a part of the experiment design.” 

*NASA GeneLab on Environmental Data*

**Environmental data is essential to a deeper understanding of the conditions the samples undergo during spaceflight**

Massive volumes of environmental data are collected during NASA spaceflight missions. Visualizations allow massive volumes of data (such as environmental data collected during spaceflight) to be easily studied, compared with data from other missions, and communicated to others. This tool was developed in 2020 by Samhita Vasu during her summer internship at NASA Genelab. As of August 2020, the tool visualizes the following environmental factors:
- Radiation
- Temperature
- CO2
- Relative Humidity

This app was built using the language R and environmental data collected during the NASA Rodent Research Missions.

## Getting Started
To run the environmental data visualization app locally, install the following.
### Installing R
[Use this link to find directions on installing R](https://cloud.r-project.org/)

[Use this link to find directions on installing RStudio](https://rstudio.com/products/rstudio/download/)
### Libraries
To install a library in R, use the following command: 
`install.packages("library_name")`

The following libraries are needed to run the app:
- shiny
- plotly
- DT
- R.utils

## Usage
First, in the RStudio console, set the working directory to the directory in which you have cloned this repository:
`setwd("directory")`

Run the app from the RStudio console:
`runApp('NASAGenelabEnvironmentalData')`

## Adding Datasets
The following are the column names (case-sensitive) necessary for the app:

**General Environmental Dataset**
- Temp_degC_ISS
- Temp_degC_Ground
- RH_percent_ISS
- RH_percent_Ground
- CO2_ppm_ISS
- CO2_ppm_Ground
- Controller_Time_GMT
- Mission_Milestone
- (*If light data available, for each light, add:*) Light1, etc
- Vehicles

**Radiation Dataset**
- Date
- GCR_Dose_mGy_d
- SAA_Dose_mGy_d
- Total_Dose_mGy_d

Copy the following code blocks and replace all occurrences of "RR1" and the RR1 radiation and environmental data files with the correct names/locations for the dataset which you desire to add. Paste into the locations specified for each code block.

*Paste the following into the beginning of app.R and comparison_helpers.R and replace "Mus musculus" with the species for your dataset*

`RR1 <- read.csv("RR1_CSV.csv", sep=",", header=TRUE)
RR1$Date_1 <- as.POSIXct(strptime(RR1$Controller_Time_GMT, '%m/%d/%Y %H:%M'))
RR1Rad <- read.csv("RR1_Radiation_CSV.csv", sep=",", header=TRUE)
RR1Rad$Date_1 <- as.POSIXct(strptime(RR1Rad$Date, '%m/%d/%y'))
i <- 1
while(i <= nrow(RR1Rad)) {
  RR1Rad[i, "Accumulation"] = sum(RR1Rad[c(1:i), "Total_Dose_mGy_d"])
  i <- i + 1
}
if(is.null(RR1$Species)) {
  RR1[1,"Species"] = "Mus musculus"
}
mission_names <- append(mission_names, "RR1")`

*Paste the following into app.R where it says "PASTE NEW TABS HERE" and replace the description/source for RR1 with those for your dataset*

`tabPanel( "RR-1", h2("Rodent Research-1"),
                                  p(em("\"This important first mission in NASA's rodent research project launched
      in 2014. It proved that the hardware system was effective and safe to use 
      in space, and that critical research operations could be carried out by 
      the space station crew. This first study showed that mice transported on 
      the SpaceX Dragon capsule, then living in the habitat on the space 
      station for a month, were both healthy and active. Rodent Research-1 also 
      included a CASIS commercial research investigation dedicated to the study 
      of muscle loss, or atrophy.\"")),
                                  p(em("- NASA Ames Research Center on RR-1")),
                                  p(tags$a(href="https://www.nasa.gov/mission_pages/station/research/news/rodent_research_complete", 
                                           "Click here for more information on NASA's RR-1 mission")),
                                  br(),
                                  h3("Overview"),
                                  fluidRow(
                                    column(8,dataTableOutput("summary_RR1")),
                                    column(4, wellPanel(
                                      htmlOutput("milestones_RR1")
                                    ))),
                                  br(),
                                  h3("The Data"),
                                  tabsetPanel(
                                    tabPanel("All Environmental Data", br(),
                                             fluidRow(
                                               column(4, wellPanel(
                                                 helpText("Select which environmental factor to display on the plots"),
                                                 checkboxGroupInput(inputId = "chosen_factor_RR1", label = NULL, choices = list("Temperature" = "Temperature", "Relative Humidity" = "Relative Humidity", "CO2" = "CO2", "Radiation accumulated" = "Radiation"), selected = "Temperature"),
                                                 helpText("Select to compare spaceflight data with ground control"),
                                                 checkboxInput(inputId = "ground_RR1", label = "Display ground control", value = FALSE),
                                                 helpText("Select to display on the timeplot the different periods of the mission (see the ", 
                                                          em("Overview"), " section for landmark date/times)"),
                                                 checkboxInput(inputId = "landmark_checkbox_RR1", label = "Display Periods", value = TRUE),
                                                 htmlOutput("show_legend_RR1"),
                                                 helpText("Select to display on the timpelot the durations that each light was switched on (Black regions are when the lights were switched on)"),
                                                 checkboxGroupInput(inputId = "light_check_group_RR1", label = NULL, choices = list("Light 1" = 1,"Light 3" = 3))
                                               )),
                                               column(8,
                                                      tabsetPanel(
                                                        tabPanel("Timeplot", br(), plotlyOutput("timeplot_RR1")), 
                                                        tabPanel("Column Chart", br(), plotlyOutput("column_chart_RR1"))
                                                      )
                                               ))), 
                                    tabPanel("Radiation Data", br(), 
                                             column(4, wellPanel(
                                               helpText("Select to display on the timeplot the different periods of the mission (see the ", 
                                                        em("Overview"), " section for landmark date/times)"),
                                               checkboxInput(inputId = "landmark_checkbox_radiation_RR1", label = "Display Periods", value = TRUE),
                                               htmlOutput("show_legend_radiation_RR1"),
                                               helpText("Select to display on the timpelot the durations that each light was switched on (Black regions are when the lights were switched on)"),
                                               checkboxGroupInput(inputId = "light_check_group_radiation_RR1", label = NULL, choices = list("Light 1" = 1,"Light 3" = 3))
                                             )),
                                             column(8, plotlyOutput("timeplot_radiation_daily_RR1"), 
                                                    br(),plotlyOutput("timeplot_radiation_accumulated_RR1"))
                                    ))),`

*Paste the following into app.R where it says "PASTE SERVER FUNCTIONS FOR NEW TABS HERE"*

`output$milestones_RR1 <- renderText({
    milestone_server(RR1)
  })
  output$timeplot_RR1 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_RR1)||is.null(input$show_legend_RR1)), FALSE,  input$show_legend_RR1)
    timeplot_server(input$chosen_factor_RR1, input$light_check_group_RR1, input$landmark_checkbox_RR1, RR1, RR1Rad, show_legend = show_legend, input$ground_RR1)
  })
  output$timeplot_radiation_daily_RR1 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_radiation_RR1)||is.null(input$show_legend_radiation_RR1)), FALSE,  input$show_legend_radiation_RR1)
    timeplot_server_daily_radiation(input$light_check_group_radiation_RR1, input$landmark_checkbox_radiation_RR1, RR1, RR1Rad, show_legend)
  })
  output$timeplot_radiation_accumulated_RR1 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_radiation_RR1)||is.null(input$show_legend_radiation_RR1)), FALSE,  input$show_legend_radiation_RR1)
    timeplot_server_accumulated_radiation(input$light_check_group_radiation_RR1, input$landmark_checkbox_radiation_RR1, RR1, RR1Rad, show_legend)
  })
  output$column_chart_RR1 <- renderPlotly({
    column_plot_server(input$chosen_factor_RR1, RR1, RR1Rad, input$ground_RR1) 
  }) 
  output$summary_RR1 <- renderDataTable({
    summary_server(RR1, RR1Rad)
  })
  output$show_legend_RR1 <- renderUI({
    if (input$landmark_checkbox_RR1) {
      checkboxInput(inputId = "show_legend_RR1", label = "Show annotations", value = TRUE)
    }
  })
  output$show_legend_radiation_RR1 <- renderUI({
    if (input$landmark_checkbox_radiation_RR1) {
      checkboxInput(inputId = "show_legend_radiation_RR1", label = "Show annotations", value = TRUE)
    }
  })`

## Contact
Samhita Vasu - svasu4@jh.edu
