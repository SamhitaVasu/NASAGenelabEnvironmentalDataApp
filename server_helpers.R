# Libraries and external file used
library(shiny)
library(plotly)
library(DT)
library(R.utils)
source("helpers.R")

# return text to display the milestones and basic information for the mission
milestone_server <- function(dataset) {
  landmark_indices <- which(dataset$Mission_Milestone != "")
  vehicle_indices <- which(dataset$Vehicles != "")
  text_list <- div(strong(gsub("_", " ", dataset[landmark_indices[1], "Mission_Milestone"]), ": "), dataset[landmark_indices[1], "Controller_Time_GMT"])
  i <- 2
  j <- 1
  while (i <= length(landmark_indices)) {
    text_list <- paste(text_list, div(strong(gsub("_", " ", dataset[landmark_indices[i], "Mission_Milestone"]), ": "), dataset[landmark_indices[i], "Controller_Time_GMT"]))
    i <- i + 1
  }
  while(j <= length(vehicle_indices)) {
    text_list <- paste(text_list, div(strong("Vehicle #",j, ": "), dataset[vehicle_indices[j], "Vehicles"]))
    j <- j + 1
  }
  text_list <- paste(text_list, div(strong("Duration: "), floor(difftime(dataset[length(dataset$Date_1), "Date_1"], dataset[1, "Date_1"])), " days"))
  return(text_list)
}

# return a summary statistics datatable for a specific rodent research mission
summary_server <- function(dataset, dataset_radiation) {
  if(missing(dataset_radiation)) {
    data_columns <- list(dataset$Temp_degC_ISS, dataset$RH_percent_ISS, dataset$CO2_ppm_ISS)
    column_names <- c("Temperature","Relative Humidity","CO2")
  }
  else {
    data_columns <- list(dataset$Temp_degC_ISS, dataset$RH_percent_ISS, dataset$CO2_ppm_ISS, dataset_radiation$Total_Dose_mGy_d,
                         dataset_radiation$GCR_Dose_mGy_d, dataset_radiation$SAA_Dose_mGy_d)
    column_names <- c("Temperature","Relative Humidity","CO2", 
                      "Radiation: Total", "Radiation: GCR", "Radiation: SAA")
  }
  stats <- create_summary_table(data_columns, column_names, 3)
  stats_transposed <- t(stats)
  datatable( stats_transposed, rownames = TRUE, options = list(dom = 't') )
}

# based on a the environmental factors, lights, and landmark(y/n) selected by the user, return the timeplot(s) for a specific rodent research mission
timeplot_server <- function(chosen_factor, light_check_group, landmark_checkbox, dataset, dataset_radiation, show_legend, ground) {
  if(missing(dataset_radiation)) {
    timeplot <- timeplot_display(chosen_factor, dataset, ground = ground)
  }
  else {
    timeplot <- timeplot_display(chosen_factor, dataset, dataset_radiation, ground) 
  }
  if(missing(show_legend)) {
    show_legend = FALSE
  }
  if(missing(light_check_group)) {
    shps_1 <- light_selector_server(dataset = dataset)
  }
  else {
    shps_1 <- light_selector_server(light_check_group, dataset)
  }
  if(missing(landmark_checkbox)) {
    shps_2 <- landmark_shape_server(FALSE, dataset)
    annotations <- landmark_annotation_server(FALSE, dataset = dataset)
  }
  else {
    shps_2 <- landmark_shape_server(landmark_checkbox, dataset)
    if(length(chosen_factor) == 1 && show_legend) {
      annotations <- switch(chosen_factor, "Temperature" = landmark_annotation_server(landmark_checkbox, shps_2, dataset, "Temp_degC_ISS"), "Relative Humidity" = landmark_annotation_server(landmark_checkbox, shps_2, dataset, "RH_percent_ISS"), "CO2" = landmark_annotation_server(landmark_checkbox, shps_2, dataset, "CO2_ppm_ISS"), "Radiation" = landmark_annotation_server(landmark_checkbox, shps_2, dataset, "Accumulation", dataset_radiation))
    }
    else if (length(chosen_factor) > 1 && show_legend) {
      annotations <- switch(chosen_factor[1], "Temperature" = landmark_annotation_server(landmark_checkbox, shps_2, dataset, "Temp_degC_ISS"), "Relative Humidity" = landmark_annotation_server(landmark_checkbox, shps_2, dataset, "RH_percent_ISS"), "CO2" = landmark_annotation_server(landmark_checkbox, shps_2, dataset, "CO2_ppm_ISS"), "Radiation" = landmark_annotation_server(landmark_checkbox, shps_2, dataset_radiation, "Accumulation", dataset_radiation))
    }
    else{ annotations <- list()}
  }
  shps <- c(shps_1, shps_2)
  if(!is.null(timeplot[[2]])) {
    if(!is.null(timeplot[[3]])) (if(!is.null(timeplot[[4]])) build_four_timeplots(timeplot, shps, annotations) else build_three_timeplots(timeplot, shps, annotations)) else build_two_timeplots(timeplot, shps, annotations)
  }
  else {
    layout(timeplot, shapes = shps, annotations = annotations)
  }
}

# based on a the lights and landmark(y/n) selected by the user, return the radiation timeplots for a specific rodent research mission
timeplot_server_daily_radiation <- function(light_check_group_radiation, landmark_checkbox_radiation, dataset, dataset_radiation, show_legend = FALSE) {
  timeplot_daily_dose <- plot_ly(data = dataset_radiation, x = ~Date_1, y= ~Total_Dose_mGy_d, type="scatter", mode = "line", name = "Total Radiation dose per day (mGy)")
  timeplot_daily_dose <- add_trace(timeplot_daily_dose, y= ~SAA_Dose_mGy_d, type="scatter", mode = "line", name = "SAA Radiation dose per day (mGy)")
  timeplot_daily_dose <- add_trace(timeplot_daily_dose, y= ~GCR_Dose_mGy_d, type="scatter", mode = "line", name = "GCR Radiation dose per day (mGy)")
  timeplot_daily_dose <- layout(timeplot_daily_dose, title = "Timeplot of Radiation Dose per Day")
  if(missing(light_check_group_radiation)) {
    shps_1 <- light_selector_server(dataset = dataset)
  }
  else {
    shps_1 <- light_selector_server(light_check_group_radiation, dataset)
  }
  shps_2 <- landmark_shape_server(landmark_checkbox_radiation, dataset)
  if (show_legend) {
    annotations <- landmark_annotation_server(landmark_checkbox_radiation, shps_2, dataset, "GCR_Dose_mGy_d", dataset_radiation, 0.07)
  }
  else{ annotations <- list()}
  shps <- c(shps_1, shps_2)
  layout(timeplot_daily_dose, shapes = shps, annotations = annotations, yaxis = list(title = "Radiation Dose (mGy)", range = c(min(as.numeric(dataset_radiation$GCR_Dose_mGy_d), as.numeric(dataset_radiation$SAA_Dose_mGy_d), as.numeric(dataset_radiation$Total_Dose_mGy_d), na.rm = TRUE), max(as.numeric(dataset_radiation$GCR_Dose_mGy_d), as.numeric(dataset_radiation$SAA_Dose_mGy_d), as.numeric(dataset_radiation$Total_Dose_mGy_d), na.rm = TRUE))), xaxis = list(title = "Date")) 
}

# based on a the lights and landmark(y/n) selected by the user, return the accumulated radiation timeplots for a specific rodent research mission
timeplot_server_accumulated_radiation <- function(light_check_group_radiation, landmark_checkbox_radiation, dataset, dataset_radiation, show_legend = FALSE) {
  timeplot_accumulated <- plot_ly(data = dataset_radiation, x = ~Date_1, y= ~Accumulation, type="scatter", mode = "line", name = "Accumulation of Radiation (mGy)")
  timeplot_accumulated <- layout(timeplot_accumulated, yaxis = list(range = c(min(as.numeric(dataset_radiation$Accumulation), na.rm = TRUE), max(as.numeric(dataset_radiation$Accumulation), na.rm = TRUE)), title = "Radiation (mGy)"), xaxis = list(title = "Date"), title = "Timeplot of Radiation Accumulated (mGy)")
  if(missing(light_check_group_radiation)) {
    shps_1 <- light_selector_server(dataset = dataset)
  }
  else {
    shps_1 <- light_selector_server(light_check_group_radiation, dataset)
  }
  shps_2 <- landmark_shape_server(landmark_checkbox_radiation, dataset)
  if (show_legend) {
    annotations <- landmark_annotation_server(landmark_checkbox_radiation, shps_2, dataset, "Accumulation", dataset_radiation)
  }
  else{ annotations <- list()}
  shps <- c(shps_1, shps_2)
  layout(timeplot_accumulated, shapes = shps, annotations = annotations)
}

# based on the environmental factor(s) selected by the user, return the column plot(s) for a specific rodent research mission
column_plot_server <- function(chosen_factor, dataset, dataset_radiation, ground) {
  landmarks <- which(dataset$Mission_Milestone != "")
  row_names <- vector(mode = "list", length = 3*(length(landmarks)+1))
  indices_1 <- get_inds("NA", dataset[landmarks[1], "Mission_Milestone"], dataset$Mission_Milestone)
  row_names[1] <- paste("Temperature: until ", dataset[landmarks[1], "Mission_Milestone"], sep = "")
  row_names[2] <- paste("RH: until ", dataset[landmarks[1], "Mission_Milestone"], sep = "")
  row_names[3] <- paste("CO2: until ", dataset[landmarks[1], "Mission_Milestone"], sep = "")
  i <- 2
  while(i <= length(landmarks)){
    var_name <- paste("indices_", i, sep = "")
    assign(var_name, get_inds(dataset[landmarks[i - 1], "Mission_Milestone"], dataset[landmarks[i], "Mission_Milestone"], dataset$Mission_Milestone))
    row_names <- insert(row_names, i, paste("Temperature: after ", dataset[landmarks[i-1], "Mission_Milestone"], sep = "", " until ", dataset[landmarks[i], "Mission_Milestone"]))
    row_names <- insert(row_names, 2*i, paste("RH: after ", dataset[landmarks[i-1], "Mission_Milestone"], sep = "", " until ", dataset[landmarks[i], "Mission_Milestone"]))
    row_names <- insert(row_names, 3*i, paste("CO2: after ", dataset[landmarks[i-1], "Mission_Milestone"], sep = "", " until ", dataset[landmarks[i], "Mission_Milestone"]))
    i <- i + 1
  }
  var_name <- paste("indices_", i, sep = "")
  assign(var_name, get_inds(dataset[landmarks[length(landmarks)], "Mission_Milestone"],"NA",  dataset$Mission_Milestone))
  row_names <- insert(row_names, i, paste("Temperature: after ", dataset[landmarks[length(landmarks)], "Mission_Milestone"], sep = ""))
  row_names <- insert(row_names, 2*i, paste("RH: after ", dataset[landmarks[length(landmarks)], "Mission_Milestone"], sep = ""))
  row_names <- insert(row_names, 3*i, paste("CO2: after ", dataset[landmarks[length(landmarks)], "Mission_Milestone"], sep = ""))
  row_names <- row_names[1:(3*(length(landmarks)+1))]
  indices <- list()
  j <- 1
  while(j <= i) {
    val <- paste("indices_", j, sep = "")
    indices[[j]] <- c(eval(as.name(paste(val))))
    j <- j +1
  }
  data_column_names <- c("Temp_degC_ISS", "RH_percent_ISS", "CO2_ppm_ISS")
  summary_statistics <- stats_for_column_chart(indices, row_names, data_column_names, dataset)
  
  color_palette <- c('#0090B8','#00A300','#F58B00','#E8000D','#751A55','#061F4C', '#397D02','#8E6D01')
  column_plot_temperature <- plot_ly(x = ~row_names[1:(length(landmarks)+1)], color = ~row_names[1:(length(landmarks)+1)], type = 'bar', marker = list(color = color_palette), name = "Temperature",  y = ~as.numeric(summary_statistics[1:(length(landmarks)+1),"mean"]),error_y = ~list(array = summary_statistics[1:(length(landmarks)+1),"standard_deviation"],color = '#000000'),  width = 800, height = 700)
  column_plot_RH <- plot_ly(x = ~row_names[(length(landmarks)+2):(2*(length(landmarks)+1))], color = ~row_names[1:(length(landmarks)+1)], marker = list(color = color_palette), y = ~as.numeric(summary_statistics[c((length(landmarks)+2):(2*(length(landmarks)+1))), "mean"]),type = 'bar', name = "RH",error_y = ~list(array =summary_statistics[c((length(landmarks)+2):(2*(length(landmarks)+1))),"standard_deviation"],color = '#000000'), width = 800, height = 700)
  column_plot_CO2 <- plot_ly(x = ~row_names[(2*(length(landmarks)+1)+1):(3*(length(landmarks)+1))], color = ~row_names[1:(length(landmarks)+1)], marker = list(color = color_palette), y = ~as.numeric(summary_statistics[c((2*(length(landmarks)+1)+1):(3*(length(landmarks)+1))), "mean"]), type = 'bar', name = "CO2",error_y = ~list(array =summary_statistics[c((2*(length(landmarks)+1)+1):(3*(length(landmarks)+1))),"standard_deviation"],color = '#000000'), width = 800, height = 700)
  
  if(ground) {
    color_palette_ground <- c("#005066","#005200","#a35c00","#a30008","#320b24","#020813","#1c3c01","#3d2f00")
    data_column_names_ground <- c("Temp_degC_Ground", "RH_percent_Ground", "CO2_ppm_Ground")
    summary_statistics_ground <- stats_for_column_chart(indices, row_names, data_column_names_ground, dataset)
    column_plot_temperature <- column_plot_temperature %>% add_trace(name = "Ground Control", marker = list(color = color_palette_ground), y = ~as.numeric(summary_statistics_ground[1:(length(landmarks)+1),"mean"]),error_y = ~list(array = summary_statistics_ground[1:(length(landmarks)+1),"standard_deviation"],color = '#000000'))
    column_plot_RH <- column_plot_RH %>% add_trace(marker = list(color = color_palette_ground), y = ~as.numeric(summary_statistics_ground[c((length(landmarks)+2):(2*(length(landmarks)+1))), "mean"]), name = "Ground Control",error_y = ~list(array =summary_statistics_ground[c((length(landmarks)+2):(2*(length(landmarks)+1))),"standard_deviation"],color = '#000000'))
    column_plot_CO2 <- column_plot_CO2 %>% add_trace(marker = list(color = color_palette_ground),y = ~as.numeric(summary_statistics_ground[c((2*(length(landmarks)+1)+1):(3*(length(landmarks)+1))), "mean"]), name = "Ground Control",error_y = ~list(array =summary_statistics_ground[c((2*(length(landmarks)+1)+1):(3*(length(landmarks)+1))),"standard_deviation"],color = '#000000'))
  }
  
  if(missing(dataset_radiation)) {}
  else{
    indices_radiation_1 <- get_inds_radiation("NA", dataset[landmarks[1], "Mission_Milestone"], dataset, dataset$Mission_Milestone, dataset_radiation)
    row_names_radiation <- vector(mode = "list", length = (length(landmarks)+1))
    row_names_radiation[1] <- paste("Radiation: until ", dataset[landmarks[1], "Mission_Milestone"], sep = "")
    i <- 2
    while(i <= length(landmarks)){
      var_name <- paste("indices_radiation_", i, sep = "")
      assign(var_name, get_inds_radiation(dataset[landmarks[i - 1], "Mission_Milestone"], dataset[landmarks[i], "Mission_Milestone"], dataset, dataset$Mission_Milestone, dataset_radiation))
      row_names_radiation <- insert(row_names_radiation, i, paste("Radiation: after ", dataset[landmarks[i-1], "Mission_Milestone"], sep = "",  " until ", dataset[landmarks[i], "Mission_Milestone"]))
      i <- i + 1
    }
    row_names_radiation <- insert(row_names_radiation, i, paste("Radiation: after ", dataset[landmarks[length(landmarks)], "Mission_Milestone"], sep = ""))
    row_names_radiation <- row_names_radiation[1:(length(landmarks)+1)]
    var_name <- paste("indices_radiation_", i, sep = "")
    assign(var_name, get_inds_radiation(dataset[landmarks[length(landmarks)], "Mission_Milestone"],"NA",  dataset, dataset$Mission_Milestone, dataset_radiation))
    indices_radiation <- list()
    j <- 1
    while(j <= i) {
      val <- paste("indices_radiation_", j, sep = "")
      indices_radiation[[j]] <- c(eval(as.name(paste(val))))
      j <- j +1
    }
    data_column_names_rad <- c("Total_Dose_mGy_d")
    summary_statistics_radiation <- stats_for_column_chart(indices_radiation, row_names_radiation, data_column_names_rad, dataset_radiation)
    column_plot_radiation <- plot_ly(x = ~row_names_radiation, color = ~row_names_radiation, marker = list(color = color_palette), y = ~summary_statistics_radiation[,"mean"], type = 'bar', name = "Radiation", error_y = ~list(array =data_radiation[,"standard_deviation"], color = '#000000'), width = 800, height = 700)
    column_plot_radiation <- column_plot_radiation %>% layout(yaxis1 = list(range = c(0, max(summary_statistics_radiation[, "mean"] + summary_statistics_radiation[, "standard_deviation"]))), title = "Radiation during different periods of the mission")
  }
  
  if(length(chosen_factor) == 1) {
    column_plot <- switch(chosen_factor, "Temperature" = column_plot_temperature, "Relative Humidity" = column_plot_RH, "CO2" = column_plot_CO2, "Radiation" = column_plot_radiation)
    column_plot <- column_plot %>% layout(yaxis = list(title = 'mean'), showlegend = FALSE, 
                                          barmode = 'group', xaxis = list(title = ''))
  }
  else if(length(chosen_factor)>1) {
    column_plot <- switch(chosen_factor[1], "Temperature" = column_plot_temperature, "Relative Humidity" = column_plot_RH, "CO2" = column_plot_CO2, "Radiation" = column_plot_radiation)
    i<-2
    while( i<=length(chosen_factor)){
      plot <- switch(chosen_factor[i], "Temperature" = column_plot_temperature, "Relative Humidity" = column_plot_RH, "CO2" = column_plot_CO2, "Radiation" = column_plot_radiation)
      column_plot <- subplot(column_plot, plot, widths = c((i-1)/i, 1/i))
      i <- i+1
    }
    column_plot <- column_plot %>% layout(yaxis = list(title ='mean'), barmode ='group',showlegend = FALSE, xaxis = list(title = ''),title = "Environmental factors during different periods of the mission")
  }
  else {
    column_plot <- plotly_empty(type = "bar")
  }
  return(column_plot)
}