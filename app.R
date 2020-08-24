# Libraries and external files used by app
library(shiny)
library(plotly)
library(DT)
source("helpers.R")
source("server_helpers.R")
source("comparison_helpers.R")

mission_names <- list()

# Read data in csv files into data frames and convert dates into proper date/time format for R
RR1 <- read.csv("RR1_CSV.csv", sep=",", header=TRUE)
RR1$Date_1 <- as.POSIXct(strptime(RR1$Controller_Time_GMT, '%m/%d/%Y %H:%M'))
RR1Rad <- read.csv("RR1_Radiation_CSV.csv", sep=",", header=TRUE)
RR1Rad$Date_1 <- as.POSIXct(strptime(RR1Rad$Date, '%m/%d/%y'))
# Calculate radiation accumulation and add calculations to new "accumulation" column in radiation data frame
i <- 1
while(i <= nrow(RR1Rad)) {
  RR1Rad[i, "Accumulation"] = sum(RR1Rad[c(1:i), "Total_Dose_mGy_d"])
  i <- i + 1
}
if(is.null(RR1$Species)) {
  RR1[1,"Species"] = "Mus musculus"
}
mission_names <- append(mission_names, "RR1")

RR3 <- read.csv("RR3_CSV.csv", sep=",", header=TRUE)
RR3$Date_1 <- as.POSIXct(strptime(RR3$Controller_Time_GMT, '%m/%d/%Y %H:%M'))
RR3Rad <- read.csv("RR3_Radiation_CSV.csv", sep=",", header=TRUE)
RR3Rad$Date_1 <- as.POSIXct(strptime(RR3Rad$Date, '%m/%d/%Y'))
i <- 1
while(i <= nrow(RR3Rad)) {
  RR3Rad[i, "Accumulation"] = sum(RR3Rad[c(1:i), "Total_Dose_mGy_d"])
  i <- i + 1
}
if(is.null(RR3$Species)) {
  RR3[1,"Species"] = "Mus musculus"
}
mission_names <- append(mission_names, "RR3")

RR5 <- read.csv("RR5_CSV.csv", sep=",", header=TRUE)
RR5$Date_1 <- as.POSIXct(strptime(RR5$Controller_Time_GMT, '%m/%d/%Y %H:%M'))
if(is.null(RR5$Species)) {
  RR5[1,"Species"] = "Mus musculus"
}
mission_names <- append(mission_names, "RR5")

RR6 <- read.csv("RR6_CSV.csv", sep=",", header=TRUE)
RR6$Date_1 <- as.POSIXct(strptime(RR6$Controller_Time_GMT, '%m/%d/%Y %H:%M'))
RR6Rad <- read.csv("RR6_Radiation_CSV.csv", sep=",", header=TRUE)
RR6Rad$Date_1 <- as.POSIXct(strptime(RR6Rad$Date, '%m/%d/%Y'))
i <- 1
while(i <= nrow(RR6Rad)) {
  RR6Rad[i, "Accumulation"] = sum(RR6Rad[c(1:i), "Total_Dose_mGy_d"])
  i <- i + 1
}
if(is.null(RR6$Species)) {
  RR6[1,"Species"] = "Mus musculus"
}
mission_names <- append(mission_names, "RR6")

RR7 <- read.csv("RR7_CSV.csv", sep=",", header=TRUE)
RR7$Date_1 <- as.POSIXct(strptime(RR7$Controller_Time_GMT, '%m/%d/%Y %H:%M'))
RR7Rad <- read.csv("RR7_Radiation_CSV.csv", sep=",", header=TRUE)
RR7Rad$Date_1 <- as.POSIXct(strptime(RR7Rad$Date, '%m/%d/%Y'))
i <- 1
while(i <= nrow(RR7Rad)) {
  RR7Rad[i, "Accumulation"] = sum(RR7Rad[c(1:i), "Total_Dose_mGy_d"])
  i <- i + 1
}
if(is.null(RR7$Species)) {
  RR7[1,"Species"] = "Mus musculus"
}
mission_names <- append(mission_names, "RR7")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    h1{padding-right: 10px;padding-left: 10px; color: #3c8dbc}
    h3{padding-right: 10px;padding-left: 10px}
    p{padding-right: 10px;padding-left: 10px}"))
  ),
  titlePanel(h1("Rodent Research Environmental Data")),
  navbarPage("NASA Genelab",
             tabPanel( "Compare", h2("Compare Environmental Data Among Datasets"),
                       br(),
                       fluidRow(
                          column(4,wellPanel(dataTableOutput('options_table')), br(),
                                 htmlOutput("graph_options")),
                          column(8, htmlOutput("graph_tabs"))
                        ),
             ),
             navbarMenu("Individual Rodent Research Missions",
                        tabPanel( "RR-1", h2("Rodent Research-1"),
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
                                    )
                                    )
                                  ),
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
                                               )
                                             )
                                    ), 
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
                                    )
                                  )
                        ),
                        tabPanel( "RR-3", h2("Rodent Research-3"),
                                  p(em("\"For the third mission, launched in 2016, CASIS formed a commercial partnership with the pharmaceutical company Eli Lilly and Company, of Indianapolis, Indiana. Their goal was to evaluate a potential new treatment for skeletal muscle wasting and weakness. Working with NASA, the researchers assessed its ability to prevent these conditions in mice exposed to long-duration spaceflight. Rodent research models have made it possible to study serious diseases involving muscle atrophy, and even age-related weakness. Conducting this study during spaceflight provided the researchers with the rare opportunity to expose all the muscles of an organism to conditions that will produce atrophy - something not easily achieved on Earth.\"")),
                                  p(em("- NASA Ames Research Center on RR-3")),
                                  p(tags$a(href="https://www.nasa.gov/mission_pages/station/research/news/rodent_research_3", 
                                           "Click here for more information on NASA's RR-3 mission")),
                                  br(),
                                  h3("Overview"),
                                  fluidRow(
                                    column(8,dataTableOutput("summary_RR3")),
                                    column(4, wellPanel(
                                      htmlOutput("milestones_RR3")
                                    )
                                    )
                                  ),
                                  br(),
                                  h3("The Data"),
                                  tabsetPanel(
                                    tabPanel("All Environmental Data", br(),
                                             fluidRow(
                                               column(4, wellPanel(
                                                 helpText("Select which environmental factor to display on the plots"),
                                                 checkboxGroupInput(inputId = "chosen_factor_RR3", label = NULL, choices = list("Temperature" = "Temperature", "Relative Humidity" = "Relative Humidity", "CO2" = "CO2", "Radiation Accumulated" = "Radiation"), selected = "Temperature"),
                                                 helpText("Select to compare spaceflight data with ground control"),
                                                 checkboxInput(inputId = "ground_RR3", label = "Display ground control", value = FALSE),
                                                 helpText("Select to display on the timeplot the different periods of the mission (see the ", 
                                                          em("Overview"), " section for landmark date/times)"),
                                                 checkboxInput(inputId = "landmark_checkbox_RR3", label = "Display Periods", value = TRUE),
                                                 htmlOutput("show_legend_RR3"),
                                                 helpText("Select to display on the timpelot the durations that each light was switched on (Black regions are when the lights were switched on)"),
                                                 checkboxGroupInput(inputId = "light_check_group_RR3", label = NULL, choices = list("Light 1" = 1,"Light 3" = 3))
                                               )),
                                               column(8,
                                                      tabsetPanel(
                                                        tabPanel("Timeplot", br(), plotlyOutput("timeplot_RR3")), 
                                                        tabPanel("Column Chart", br(), plotlyOutput("column_chart_RR3"))
                                                      )
                                               )
                                             )
                                    ), 
                                    tabPanel("Radiation Data", br(), 
                                             column(4, wellPanel(
                                               helpText("Select to display on the timeplot the different periods of the mission (see the ", 
                                                        em("Overview"), " section for landmark date/times)"),
                                               checkboxInput(inputId = "landmark_checkbox_radiation_RR3", label = "Display Periods", value = TRUE),
                                               htmlOutput("show_legend_radiation_RR3"),
                                               helpText("Select to display on the timpelot the durations that each light was switched on (Black regions are when the lights were switched on)"),
                                               checkboxGroupInput(inputId = "light_check_group_radiation_RR3", label = NULL, choices = list("Light 1" = 1,"Light 3" = 3))
                                             )),
                                             column(8, plotlyOutput("timeplot_radiation_daily_RR3"), 
                                                    br(),plotlyOutput("timeplot_radiation_accumulated_RR3"))
                                    )
                                  )
                        ),
                        tabPanel( "RR-5", h2("Rodent Research-5"),
                                  p(em("\"The topic of study on this mission was bone tissue loss - a challenge for the health of astronauts, as well as for people on Earth suffering from osteoporosis. For the experiment, a group of mice was launched to the space station in June 2017. Half of them returned to Earth one month later, while the other half remained in space for a full two months. This allowed the researchers - a team based at University of California, Los Angeles - to study the effects of a potential treatment for bone loss, and how the body readapts to Earth conditions after an extended stay in microgravity.\"")),
                                  p(em("- NASA Ames Research Center on RR-5")),
                                  p(tags$a(href="https://www.nasa.gov/mission_pages/station/research/experiments/explorer/Investigation.html?#id=2017", 
                                           "Click here for more information on NASA's RR-5 mission")),
                                  br(),
                                  h3("Overview"),
                                  fluidRow(
                                    column(8,dataTableOutput("summary_RR5")),
                                    column(4, wellPanel(
                                      htmlOutput("milestones_RR5")
                                    )
                                    )
                                  ),
                                  br(),
                                  h3("The Data"),
                                  tabsetPanel(
                                    tabPanel("All Environmental Data", br(),
                                             fluidRow(
                                               column(4, wellPanel(
                                                 helpText("Select which environmental factor to display on the plots"),
                                                 checkboxGroupInput(inputId = "chosen_factor_RR5", label = NULL, choices = list("Temperature" = "Temperature", "Relative Humidity" = "Relative Humidity", "CO2" = "CO2"), selected = "Temperature"),
                                                 helpText("Select to compare spaceflight data with ground control"),
                                                 checkboxInput(inputId = "ground_RR5", label = "Display ground control", value = FALSE),
                                                 helpText("Select to display on the timeplot the different periods of the mission (see the ", 
                                                          em("Overview"), " section for landmark date/times)"),
                                                 checkboxInput(inputId = "landmark_checkbox_RR5", label = "Display Periods", value = TRUE),
                                                 htmlOutput("show_legend_RR5"),
                                                 # Uncomment when light data added for RR5
                                                 # helpText("Select to display on the timpelot the durations that each light was switched on (Black regions are when the lights were switched on)"),
                                                 # checkboxGroupInput(inputId = "light_check_group_RR5", label = NULL, choices = list("Light 1" = 1,"Light 3" = 3))
                                               )),
                                               column(8,
                                                      tabsetPanel(
                                                        tabPanel("Timeplot", br(), plotlyOutput("timeplot_RR5")), 
                                                        tabPanel("Column Chart", br(), plotlyOutput("column_chart_RR5"))
                                                      )
                                               )
                                             )
                                    ) 
                                    # tabPanel("Radiation Data", br(), 
                                    # Reuse the RR1 radiation panel and make minor changes when adding radiation data for this mission to the app
                                    #          
                                    # )
                                  )
                        ),
                        tabPanel( "RR-6", h2("Rodent Research-6"),
                                  p(em("\"The topic of muscle wasting, or atrophy, was studied in Rodent Research-6. This is a problem for astronauts, brought on by microgravity, and also for many patients on Earth in different situations of disease or limited exercise. Working with NASA and CASIS, researchers from the pharmaceutical company Novartis, NanoMedical Systems and Houston Methodist Research Institute evaluated a new system for administering a drug that might be able to counteract this effect. A tiny chip, implanted under the skin, delivers a constant, low dose of the drug being tested, helping also to avoid the known side effects of its long-term use at high doses.\"")),
                                  p(em("- NASA Ames Research Center on RR-6")),
                                  p(tags$a(href="https://www.nasa.gov/ames/research/space-biosciences/rodent-research-6-spacex-13", 
                                           "Click here for more information on NASA's RR-6 mission")),
                                  br(),
                                  h3("Overview"),
                                  fluidRow(
                                    column(8,dataTableOutput("summary_RR6")),
                                    column(4, wellPanel(
                                      htmlOutput("milestones_RR6")
                                    )
                                    )
                                  ),
                                  br(),
                                  h3("The Data"),
                                  tabsetPanel(
                                    tabPanel("All Environmental Data", br(),
                                             fluidRow(
                                               column(4, wellPanel(
                                                 helpText("Select which environmental factor to display on the plots"),
                                                 checkboxGroupInput(inputId = "chosen_factor_RR6", label = NULL, choices = list("Temperature" = "Temperature", "Relative Humidity" = "Relative Humidity", "CO2" = "CO2", "Radiation Accumulated" = "Radiation"), selected = "Temperature"),
                                                 helpText("Select to compare spaceflight data with ground control"),
                                                 checkboxInput(inputId = "ground_RR6", label = "Display ground control", value = FALSE),
                                                 helpText("Select to display on the timeplot the different periods of the mission (see the ", 
                                                          em("Overview"), " section for landmark date/times)"),
                                                 checkboxInput(inputId = "landmark_checkbox_RR6", label = "Display Periods", value = TRUE),
                                                 htmlOutput("show_legend_RR6"),
                                                 # Uncomment when light data added for RR6
                                                 # helpText("Select to display on the timpelot the durations that each light was switched on (Black regions are when the lights were switched on)"),
                                                 # checkboxGroupInput(inputId = "light_check_group_RR6", label = NULL, choices = list("Light 1" = 1,"Light 3" = 3))
                                               )),
                                               column(8,
                                                      tabsetPanel(
                                                        tabPanel("Timeplot", br(), plotlyOutput("timeplot_RR6")), 
                                                        tabPanel("Column Chart", br(), plotlyOutput("column_chart_RR6"))
                                                      )
                                               )
                                             )
                                    ),
                                    tabPanel("Radiation Data", br(), 
                                             column(4, wellPanel(
                                               helpText("Select to display on the timeplot the different periods of the mission (see the ", 
                                                        em("Overview"), " section for landmark date/times)"),
                                               checkboxInput(inputId = "landmark_checkbox_radiation_RR6", label = "Display Periods", value = TRUE),
                                               htmlOutput("show_legend_radiation_RR6"),
                                               # Uncomment when light data added for RR6
                                               # helpText("Select to display on the timpelot the durations that each light was switched on (Black regions are when the lights were switched on)"),
                                               # checkboxGroupInput(inputId = "light_check_group_radiation_RR6", label = NULL, choices = list("Light 1" = 1,"Light 3" = 3))
                                             )),
                                             column(8, plotlyOutput("timeplot_radiation_daily_RR6"), 
                                                    br(),plotlyOutput("timeplot_radiation_accumulated_RR6"))
                                    )
                                  )
                        ),
                        tabPanel( "RR-7", h2("Rodent Research-7"),
                                  p(em("\"Whole communities of microorganisms live on and in our bodies, and they play an important role in human health. Known collectively as the microbiome, these communities are likely to be affected by the unique conditions of space, including microgravity. Rodent Research-7 looked at how the reduced-gravity environment of the International Space Station affects the community of microorganisms in the gut and what impact this has on multiple systems in the body. The results will show scientists how they might create solutions to keep astronauts safe and healthy on long-duration spaceflight missions and improve the treatment of gastrointestinal, immune, metabolic and sleep disorders on Earth.\"")),
                                  p(em("- NASA Ames Research Center on RR-7")),
                                  p(tags$a(href="https://www.nasa.gov/ames/research/space-biosciences/rodent-research-7", 
                                           "Click here for more information on NASA's RR-7 mission")),
                                  br(),
                                  h3("Overview"),
                                  fluidRow(
                                    column(8,dataTableOutput("summary_RR7")),
                                    column(4, wellPanel(
                                      htmlOutput("milestones_RR7")
                                    )
                                    )
                                  ),
                                  br(),
                                  h3("The Data"),
                                  tabsetPanel(
                                    tabPanel("All Environmental Data", br(),
                                             fluidRow(
                                               column(4, wellPanel(
                                                 helpText("Select which environmental factor to display on the plots"),
                                                 checkboxGroupInput(inputId = "chosen_factor_RR7", label = NULL, choices = list("Temperature" = "Temperature", "Relative Humidity" = "Relative Humidity", "CO2" = "CO2", "Radiation Accumulated" = "Radiation"), selected = "Temperature"),
                                                 helpText("Select to compare spaceflight data with ground control"),
                                                 checkboxInput(inputId = "ground_RR7", label = "Display ground control", value = FALSE),
                                                 helpText("Select to display on the timeplot the different periods of the mission (see the ", 
                                                          em("Overview"), " section for landmark date/times)"),
                                                 checkboxInput(inputId = "landmark_checkbox_RR7", label = "Display Periods", value = TRUE),
                                                 htmlOutput("show_legend_RR7"),
                                                 # Uncomment when light data added for RR7
                                                 # helpText("Select to display on the timpelot the durations that each light was switched on (Black regions are when the lights were switched on)"),
                                                 # checkboxGroupInput(inputId = "light_check_group_RR7", label = NULL, choices = list("Light 1" = 1,"Light 3" = 3))
                                               )),
                                               column(8,
                                                      tabsetPanel(
                                                        tabPanel("Timeplot", br(), plotlyOutput("timeplot_RR7")), 
                                                        tabPanel("Column Chart", br(), plotlyOutput("column_chart_RR7"))
                                                      )
                                               )
                                             )
                                    ),
                                    tabPanel("Radiation Data", br(), 
                                             column(4, wellPanel(
                                               helpText("Select to display on the timeplot the different periods of the mission (see the ", 
                                                        em("Overview"), " section for landmark date/times)"),
                                               checkboxInput(inputId = "landmark_checkbox_radiation_RR7", label = "Display Periods", value = TRUE),
                                               htmlOutput("show_legend_radiation_RR7"),
                                               # Uncomment when light data added for RR7
                                               # helpText("Select to display on the timpelot the durations that each light was switched on (Black regions are when the lights were switched on)"),
                                               # checkboxGroupInput(inputId = "light_check_group_radiation_RR7", label = NULL, choices = list("Light 1" = 1,"Light 3" = 3))
                                             )),
                                             column(8, plotlyOutput("timeplot_radiation_daily_RR7"), 
                                                    br(),plotlyOutput("timeplot_radiation_accumulated_RR7"))
                                    )
                                  )
                        ) 
                        # PASTE NEW TABS HERE
                        )
  ),
  br(),
  br()
)

server <- function(input, output) {
  output$options_table <- renderDataTable({
    table <-  matrix(ncol = 1, nrow = length(mission_names))
    rownames(table) <- mission_names
    colnames(table) <- c("Species")
    table[,"Species"] <- sapply(mission_names, function(x) {get(x)[1, "Species"]})
    datatable(table, rownames = TRUE, options = list(dom = 't'))
  })
  
  output$graph_options <- renderUI({
    if (length(input$options_table_rows_selected)) {
      wellPanel(
        helpText("Select which visualization to display"),
        checkboxGroupInput(inputId = "chosen_factor_comparison", label = NULL, choices = list("Timeplot" = "Timeplot", "Boxplot" = "Boxplot", "Bar Chart" = "Bar Chart"), selected = list("Timeplot" = "Timeplot", "Boxplot" = "Boxplot", "Bar Chart" = "Bar Chart")),
        htmlOutput("comparison_landmarks_checkbox"),
        htmlOutput("shift_to_transfer_checkbox")
    )
    }
  })
  
  output$comparison_landmarks_checkbox <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, "Timeplot")) {
      wellPanel(checkboxInput(inputId = "show_landmarks_comparison", label = "Show landmarks", value = TRUE),
               helpText("Select to display landmarks on timeplot"))
    }
  })
  
  output$shift_to_transfer_checkbox <- renderUI({
    if(length(input$chosen_factor_comparison)) {
      wellPanel(checkboxInput(inputId = "after_transfer", label = "Shift data", value = TRUE),
                helpText("Select to set 'Day 0' as when animal transfer happened"))
    }
  })
  
  output$graph_tabs <- renderUI({
    if (length(input$options_table_rows_selected)) {
      tabsetPanel(
        tabPanel("Radiation", br(), htmlOutput("comparison_timeplots_radiation"),
                 htmlOutput("comparison_boxplots_radiation"),
                 htmlOutput("comparison_columnplot_radiation")),
        tabPanel("Temperature", br(), htmlOutput("comparison_timeplots_temperature"),
                 htmlOutput("comparison_boxplots_temperature"),
                 htmlOutput("comparison_columnplot_temperature")),
        tabPanel("Relative Humidity", br(), htmlOutput("comparison_timeplots_RH"),
                 htmlOutput("comparison_boxplots_RH"),
                 htmlOutput("comparison_columnplot_RH")),
        tabPanel("CO2", br(), htmlOutput("comparison_timeplots_CO2"),
                 htmlOutput("comparison_boxplots_CO2"),
                 htmlOutput("comparison_columnplot_CO2"))
      )
    }
  })
  
  output$comparison_boxplots_temperature <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Boxplot')) {
      fluidRow(plotlyOutput("comparison_boxplot_temperature_space"), br(), hr(), br(),
               plotlyOutput("comparison_boxplot_temperature_ground"), br(), hr(), br())
    }
  })
  
  output$comparison_boxplot_temperature_space <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_boxplot_server("Temp_degC_ISS", rows, input$after_transfer)
    }
  })
  
  output$comparison_boxplot_temperature_ground <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_boxplot_server("Temp_degC_Ground", rows, input$after_transfer)
    }
  })
  
  output$comparison_columnplot_temperature <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Bar Chart')) {
      fluidRow(plotlyOutput("comparison_columnplot_temperature_both"), br(), hr(), br())
    }
  })
  
  output$comparison_columnplot_temperature_both <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_columnplot_server("Temp_degC_ISS","Temp_degC_Ground", rows, input$after_transfer)
    }
  })
  
  output$comparison_timeplots_temperature <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Timeplot')) {
      fluidRow(plotlyOutput("comparison_timeplot_temperature_space"), br(), hr(), br(),
               plotlyOutput("comparison_timeplot_temperature_ground"), br(), hr(), br())
    }
  })
  
  output$comparison_timeplot_temperature_space <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_timeplot_server("Temp_degC_ISS", rows, input$show_landmarks_comparison, input$after_transfer)
    }
  })
  
  output$comparison_timeplot_temperature_ground <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_timeplot_server("Temp_degC_Ground", rows,input$show_landmarks_comparison, input$after_transfer)
    }
  })
  
  output$comparison_boxplots_RH <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Boxplot')) {
      fluidRow(plotlyOutput("comparison_boxplot_RH_space"), br(), hr(), br(),
               plotlyOutput("comparison_boxplot_RH_ground"), br(), hr(), br())
    }
  })
  
  output$comparison_boxplot_RH_space <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_boxplot_server("RH_percent_ISS", rows, input$after_transfer)
    }
  })
  
  output$comparison_boxplot_RH_ground <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_boxplot_server("RH_percent_Ground", rows, input$after_transfer)
    }
  })
  
  output$comparison_columnplot_RH <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Bar Chart')) {
      fluidRow(plotlyOutput("comparison_columnplot_RH_both"), br(), hr(), br())
    }
  })
  
  output$comparison_columnplot_RH_both <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_columnplot_server("RH_percent_ISS","RH_percent_Ground", rows, input$after_transfer)
    }
  })
  
  output$comparison_timeplots_RH <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Timeplot')) {
      fluidRow(plotlyOutput("comparison_timeplot_RH_space"), br(), hr(), br(),
               plotlyOutput("comparison_timeplot_RH_ground"), br(), hr(), br())
    }
  })
  
  output$comparison_timeplot_RH_space <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_timeplot_server("RH_percent_ISS", rows,input$show_landmarks_comparison, input$after_transfer)
    }
  })
  
  output$comparison_timeplot_RH_ground <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_timeplot_server("RH_percent_Ground", rows,input$show_landmarks_comparison, input$after_transfer)
    }
  })
  
  output$comparison_boxplots_CO2 <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Boxplot')) {
      fluidRow(plotlyOutput("comparison_boxplot_CO2_space"), br(), hr(), br(),
               plotlyOutput("comparison_boxplot_CO2_ground"), br(), hr(), br())
    }
  })
  
  output$comparison_boxplot_CO2_space <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_boxplot_server("CO2_ppm_ISS", rows, input$after_transfer)
    }
  })
  
  output$comparison_boxplot_CO2_ground <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_boxplot_server("CO2_ppm_Ground", rows, input$after_transfer)
    }
  })
  
  output$comparison_columnplot_CO2 <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Bar Chart')) {
      fluidRow(plotlyOutput("comparison_columnplot_CO2_both"), br(), hr(), br())
    }
  })
  
  output$comparison_columnplot_CO2_both <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_columnplot_server("CO2_ppm_ISS","CO2_ppm_Ground", rows, input$after_transfer)
    }
  })
  
  output$comparison_timeplots_CO2 <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Timeplot')) {
      fluidRow(plotlyOutput("comparison_timeplot_CO2_space"), br(), hr(), br(),
               plotlyOutput("comparison_timeplot_CO2_ground"), br(), hr(), br())
    }
  })
  
  output$comparison_timeplot_CO2_space <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_timeplot_server("CO2_ppm_ISS", rows,input$show_landmarks_comparison, input$after_transfer)
    }
  })

  output$comparison_timeplot_CO2_ground <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_timeplot_server("CO2_ppm_Ground", rows,input$show_landmarks_comparison, input$after_transfer)
    }
  })
  
  output$comparison_boxplots_radiation <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Boxplot')) {
      fluidRow(plotlyOutput("comparison_boxplot_radiation_daily"), br(), hr(), br())
    }
  })
  
  output$comparison_boxplot_radiation_accumulated <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_boxplot_server_radiation("Accumulation", rows, input$after_transfer)
    }
  })
  
  output$comparison_boxplot_radiation_daily <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_boxplot_server_radiation("Total_Dose_mGy_d", rows, input$after_transfer)
    }
  })
  
  output$comparison_columnplot_radiation <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Bar Chart')) {
      fluidRow(plotlyOutput("comparison_columnplot_radiation_daily"), br(), hr(), br())
    }
  })
  
  output$comparison_columnplot_radiation_daily <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_columnplot_server_radiation("Total_Dose_mGy_d", rows, input$after_transfer)
    }
  })
  
  output$comparison_timeplots_radiation <- renderUI({
    if(if_comparison_factor_selected(input$chosen_factor_comparison, 'Timeplot')) {
      fluidRow(plotlyOutput("comparison_timeplot_radiation_accumulated"), br(), hr(), br(),
               plotlyOutput("comparison_timeplot_radiation_daily"), br(), hr(), br())
    }
  })
  
  output$comparison_timeplot_radiation_accumulated <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_timeplot_server_radiation("Accumulation", rows,input$show_landmarks_comparison, input$after_transfer)
    }
  })
  
  output$comparison_timeplot_radiation_daily <- renderPlotly({
    s <- input$options_table_rows_selected
    rows <- mission_names[as.numeric(s)]
    if (length(rows)) {
      comparison_timeplot_server_radiation("Total_Dose_mGy_d", rows, input$show_landmarks_comparison, input$after_transfer)
    }
  })
  
  ######
  # RR1
  ######
  output$milestones_RR1 <- renderText({
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
  # general summary table
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
  })
  
  ######
  # RR3
  ######
  output$milestones_RR3 <- renderText({
    milestone_server(RR3)
  })
  output$timeplot_RR3 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_RR3)||is.null(input$show_legend_RR3)), FALSE,  input$show_legend_RR3)
    timeplot_server(input$chosen_factor_RR3, input$light_check_group_RR3, input$landmark_checkbox_RR3, RR3, RR3Rad, show_legend = show_legend, ground = input$ground_RR3)
  })
  output$timeplot_radiation_daily_RR3 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_radiation_RR3)||is.null(input$show_legend_radiation_RR3)), FALSE,  input$show_legend_radiation_RR3)
    timeplot_server_daily_radiation(input$light_check_group_radiation_RR3, input$landmark_checkbox_radiation_RR3, RR3, RR3Rad, show_legend)
  })
  output$timeplot_radiation_accumulated_RR3 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_radiation_RR3)||is.null(input$show_legend_radiation_RR3)), FALSE,  input$show_legend_radiation_RR3)
    timeplot_server_accumulated_radiation(input$light_check_group_radiation_RR3, input$landmark_checkbox_radiation_RR3, RR3, RR3Rad, show_legend)
  })
  output$column_chart_RR3 <- renderPlotly({
    column_plot_server(input$chosen_factor_RR3, RR3, RR3Rad, input$ground_RR3) 
  }) 
  # general summary table
  output$summary_RR3 <- renderDataTable({
    summary_server(RR3)
  })
  output$show_legend_RR3 <- renderUI({
    if (input$landmark_checkbox_RR3) {
      checkboxInput(inputId = "show_legend_RR3", label = "Show annotations", value = TRUE)
    }
  })
  output$show_legend_radiation_RR3 <- renderUI({
    if (input$landmark_checkbox_radiation_RR3) {
      checkboxInput(inputId = "show_legend_radiation_RR3", label = "Show annotations", value = TRUE)
    }
  })
  
  ######
  # RR5
  ######
  output$milestones_RR5 <- renderText({
    milestone_server(RR5)
  })
  output$timeplot_RR5 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_RR5)||is.null(input$show_legend_RR5)), FALSE,  input$show_legend_RR5)
    # add input$light_check_group_RR5 when light data added for RR5 (in between chosen factor and landmark checkbox)
    timeplot_server(input$chosen_factor_RR5, landmark_checkbox = input$landmark_checkbox_RR5, dataset = RR5, show_legend = show_legend, ground = input$ground_RR5)
  })
  output$column_chart_RR5 <- renderPlotly({
    column_plot_server(input$chosen_factor_RR5, RR5, ground = input$ground_RR5) 
  }) 
  # general summary table
  output$summary_RR5 <- renderDataTable({
    summary_server(RR5)
  })
  output$show_legend_RR5 <- renderUI({
    if (input$landmark_checkbox_RR5) {
      checkboxInput(inputId = "show_legend_RR5", label = "Show annotations", value = TRUE)
    }
  })
  
  ######
  # RR6
  ######
  output$milestones_RR6 <- renderText({
    milestone_server(RR6)
  })
  output$timeplot_RR6 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_RR6)||is.null(input$show_legend_RR6)), FALSE,  input$show_legend_RR6)
    # add input$light_check_group_RR6 when light data added for RR5 (in between chosen factor and landmark checkbox)
    timeplot_server(input$chosen_factor_RR6, landmark_checkbox = input$landmark_checkbox_RR6, dataset = RR6, dataset_radiation = RR6Rad, show_legend = show_legend, ground = input$ground_RR6)
  })
  output$timeplot_radiation_daily_RR6 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_radiation_RR6)||is.null(input$show_legend_radiation_RR6)), FALSE,  input$show_legend_radiation_RR6)
    timeplot_server_daily_radiation(landmark_checkbox_radiation = input$landmark_checkbox_radiation_RR6, dataset = RR6, dataset_radiation = RR6Rad, show_legend = show_legend)
  })
  output$timeplot_radiation_accumulated_RR6 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_radiation_RR6)||is.null(input$show_legend_radiation_RR6)), FALSE,  input$show_legend_radiation_RR6)
    timeplot_server_accumulated_radiation(landmark_checkbox_radiation = input$landmark_checkbox_radiation_RR6, dataset = RR6, dataset_radiation = RR6Rad, show_legend = show_legend)
  })
  output$column_chart_RR6 <- renderPlotly({
    column_plot_server(input$chosen_factor_RR6, RR6, RR6Rad, input$ground_RR6) 
  }) 
  # general summary table
  output$summary_RR6 <- renderDataTable({
    summary_server(RR6)
  })
  output$show_legend_RR6 <- renderUI({
    if (input$landmark_checkbox_RR6) {
      checkboxInput(inputId = "show_legend_RR6", label = "Show annotations", value = TRUE)
    }
  })
  output$show_legend_radiation_RR6 <- renderUI({
    if (input$landmark_checkbox_radiation_RR6) {
      checkboxInput(inputId = "show_legend_radiation_RR6", label = "Show annotations", value = TRUE)
    }
  })
  
  ######
  # RR7
  ######
  output$milestones_RR7 <- renderText({
    milestone_server(RR7)
  })
  output$timeplot_RR7 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_RR7)||is.null(input$show_legend_RR7)), FALSE,  input$show_legend_RR7)
    # add input$light_check_group_RR6 when light data added for RR5 (in between chosen factor and landmark checkbox)
    timeplot_server(input$chosen_factor_RR7, landmark_checkbox = input$landmark_checkbox_RR7, dataset = RR7, dataset_radiation = RR7Rad, show_legend = show_legend, ground = input$ground_RR7)
  })
  output$timeplot_radiation_daily_RR7 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_radiation_RR7)||is.null(input$show_legend_radiation_RR7)), FALSE,  input$show_legend_radiation_RR7)
    timeplot_server_daily_radiation(landmark_checkbox_radiation = input$landmark_checkbox_radiation_RR7, dataset = RR7, dataset_radiation = RR7Rad, show_legend = show_legend)
  })
  output$timeplot_radiation_accumulated_RR7 <- renderPlotly({
    show_legend <- ifelse((is.na(input$show_legend_radiation_RR7)||is.null(input$show_legend_radiation_RR7)), FALSE,  input$show_legend_radiation_RR7)
    timeplot_server_accumulated_radiation(landmark_checkbox_radiation = input$landmark_checkbox_radiation_RR7, dataset = RR7, dataset_radiation = RR7Rad, show_legend = show_legend)
  })
  output$column_chart_RR7 <- renderPlotly({
    column_plot_server(input$chosen_factor_RR7, RR7, RR7Rad, input$ground_RR7) 
  }) 
  # general summary table
  output$summary_RR7 <- renderDataTable({
    summary_server(RR7)
  })
  output$show_legend_RR7 <- renderUI({
    if (input$landmark_checkbox_RR7) {
      checkboxInput(inputId = "show_legend_RR7", label = "Show annotations", value = TRUE)
    }
  })
  output$show_legend_radiation_RR7 <- renderUI({
    if (input$landmark_checkbox_radiation_RR7) {
      checkboxInput(inputId = "show_legend_radiation_RR7", label = "Show annotations", value = TRUE)
    }
  })
  
  # PASTE SERVER FUNCTIONS FOR NEW TABS HERE
}

shinyApp(ui, server)