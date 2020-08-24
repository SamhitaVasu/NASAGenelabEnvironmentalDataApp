# library used
library(plotly)

# return the timeplots of the environmental factors selected by the user for a specific rodent research mission
timeplot_display <- function(chosen_factor, dataset, dataset_radiation, ground) {
  timeplot_temperature <- plot_ly(data = dataset, x = ~Date_1, y = ~as.numeric(dataset$Temp_degC_ISS), type="scatter", mode = "line", name = "Spaceflight Temperature (Celsius)") 
  timeplot_temperature <- layout(timeplot_temperature, yaxis = list(range = c(min(c(as.numeric(dataset$Temp_degC_ISS), as.numeric(dataset$Temp_degC_Ground)), na.rm = TRUE), max(c(as.numeric(dataset$Temp_degC_ISS), as.numeric(dataset$Temp_degC_Ground)), na.rm = TRUE)), title = "Temperature"), title = "Timeplot of Temperature (Celsius)")
  timeplot_RH <- plot_ly(data = dataset, x = ~Date_1, y = ~as.numeric(dataset$RH_percent_ISS), type="scatter", mode = "line", name = "Spaceflight Relative Humidity (%)") 
  timeplot_RH <- layout(timeplot_RH, yaxis = list(range = c(min(c(as.numeric(dataset$RH_percent_ISS), as.numeric(dataset$RH_percent_Ground)), na.rm = TRUE), max(c(as.numeric(dataset$RH_percent_ISS), as.numeric(dataset$RH_percent_Ground)), na.rm = TRUE)), title = "Humidity"), title = "Timeplot of Relative Humidity (%)")
  timeplot_CO2 <- plot_ly(data = dataset, x = ~Date_1, y = ~as.numeric(dataset$CO2_ppm_ISS), type="scatter", mode = "line", name = "Spaceflight CO2 (ppm)") 
  timeplot_CO2 <- layout(timeplot_CO2, yaxis = list(range = c(min(c(as.numeric(dataset$CO2_ppm_ISS), as.numeric(dataset$CO2_ppm_Ground)), na.rm = TRUE), max(c(as.numeric(dataset$CO2_ppm_ISS), as.numeric(dataset$CO2_ppm_Ground)), na.rm = TRUE)), title = "CO2"),title = "Timeplot of CO2 (ppm)")
  if(ground) {
    timeplot_temperature <- add_trace(timeplot_temperature, y = ~as.numeric(dataset$Temp_degC_Ground), name = "Ground Control Temperature (Celsius)")    
    timeplot_RH <- add_trace(timeplot_RH, y = ~as.numeric(dataset$RH_percent_Ground), name = "Ground Control Relative Humidity (%)")
    timeplot_CO2 <- add_trace(timeplot_CO2, y = ~as.numeric(dataset$CO2_ppm_Ground), name = "Ground Control Relative CO2 (ppm)")
  }
  if(!missing(dataset_radiation)) {
    timeplot_radiation <- plot_ly(data = dataset_radiation, x = ~Date_1, y= ~Accumulation, type="scatter", mode = "line", name = "Accumulation of Radiation during spaceflight(mGy)")
    timeplot_radiation <- layout(timeplot_radiation, yaxis = list(range = c(min(as.numeric(dataset_radiation$Accumulation), na.rm = TRUE), max(as.numeric(dataset_radiation$Accumulation), na.rm = TRUE)),title = "Radiation"),title = "Timeplot of Radiation Accumulated during spaceflight (mGy)") 
  }
  else {
    timeplot_radiation <- plotly_empty(type = "scatter", mode = "markers")
  }
  timeplot <- plotly_empty(type = "scatter", mode = "markers")
  if(length(chosen_factor) == 1) {
    timeplot <- switch(chosen_factor, "Temperature" = timeplot_temperature, "Relative Humidity" = timeplot_RH, "CO2" = timeplot_CO2, "Radiation" = timeplot_radiation)
    timeplot <- layout(timeplot, xaxis = list(title = "Date"))
  }
  else if (length(chosen_factor) > 1) {
    i<-1
    while( i<=length(chosen_factor)){
      timeplot[[i]] <- switch(chosen_factor[i], "Temperature" = timeplot_temperature, "Relative Humidity" = timeplot_RH, "CO2" = timeplot_CO2, "Radiation" = timeplot_radiation)
      i <- i+1
    }
  }
  else {
    timeplot <- plotly_empty(type = "scatter", mode = "markers")
  }
  
  return(timeplot)
}

# for the lights selected by the user, return a list of plotly shapes based on when the lights were turned on in a given rodent research mission
light_selector_server <- function(light_check_group, dataset) {
  if(missing(light_check_group)) {
    return(c())
  }
  #For light shading
  light_1_value <- dataset[1, "Light1"]
  light_2_value <- dataset[1, "Light2"]
  light_3_value <- dataset[1, "Light3"]
  on_1_index <- 1
  off_1_index <- 1
  on_2_index <- 1
  off_2_index <- 1
  on_3_index <- 1
  off_3_index <- 1
  if(light_1_value == "On") {
    on_1_index <- on_1_index + 1
  }
  if(light_2_value == "On") {
    on_2_index <- on_2_index + 1
  }
  if(light_3_value == "On") {
    on_3_index <- on_3_index + 1
  }
  on_1 <- c(dataset[1, "Controller_Time_GMT"])
  on_2 <- c(dataset[1, "Controller_Time_GMT"])
  on_3 <- c(dataset[1, "Controller_Time_GMT"])
  off_1 <- c(dataset[1, "Controller_Time_GMT"])
  off_2 <- c(dataset[1, "Controller_Time_GMT"])
  off_3 <- c(dataset[1, "Controller_Time_GMT"])
  i <- 1
  while(i < nrow(dataset)) {
    if(dataset[i, "Light1"] != light_1_value) {
      if(dataset[i, "Light1"] == "On") {
        on_1[on_1_index] <- dataset[i, "Controller_Time_GMT"]
        on_1_index <- on_1_index + 1
      }
      else if(dataset[i, "Light1"] == "Off") {
        off_1[off_1_index] <- dataset[i, "Controller_Time_GMT"]
        off_1_index <- off_1_index + 1
      }
      light_1_value <- dataset[i, "Light1"]
    }
    if(dataset[i, "Light2"] != light_2_value) {
      if(dataset[i, "Light2"] == "On") {
        on_2[on_2_index] <- dataset[i, "Controller_Time_GMT"]
        on_2_index <- on_2_index + 1
      }
      else if(dataset[i, "Light2"] == "Off") {
        off_2[off_2_index] <- dataset[i, "Controller_Time_GMT"]
        off_2_index <- off_2_index + 1
      }
      light_2_value <- dataset[i, "Light2"]
    }
    if(dataset[i, "Light3"] != light_3_value) {
      if(dataset[i, "Light3"] == "On") {
        on_3[on_3_index] <- dataset[i, "Controller_Time_GMT"]
        on_3_index <-on_3_index + 1
      }
      else if(dataset[i, "Light3"] == "Off") {
        off_3[off_3_index] <- dataset[i, "Controller_Time_GMT"]
        off_3_index <- off_3_index + 1
      }
      light_3_value <- dataset[i, "Light3"]
    }
    i <- i+1
  }
  if(length(on_1) != length(off_1)) {
    length(on_1) <- max(length(on_1), length(off_1))
    length(off_1) <- max(length(on_1), length(off_1))
    on_1[which(is.na(on_1))] <- off_1[which(is.na(on_1))-1]
    off_1[which(is.na(off_1))] <- ifelse(which(is.na(off_1)) == length(off_1), dataset[length(dataset$Controller_Time_GMT), "Controller_Time_GMT"], on_1[which(is.na(off_1))+1])
  }
  if(length(on_2) != length(off_2)) {
    length(on_2) <- max(length(on_2), length(off_2))
    length(off_2) <-  max(length(on_2), length(off_2))
    on_2[which(is.na(on_2))] <- off_2[which(is.na(on_2))-1]
    off_2[which(is.na(off_2))] <- ifelse(which(is.na(off_2)) == length(off_2), dataset[length(dataset$Controller_Time_GMT), "Controller_Time_GMT"], on_2[which(is.na(off_2))+1])
  }
  if(length(on_3) != length(off_3)) {
    length(on_3) <- max(length(on_3), length(off_3))
    length(off_3) <-  max(length(on_3), length(off_3))
    on_3[which(is.na(on_3))] <- off_3[which(is.na(on_3))-1]
    off_3[which(is.na(off_3))] <- ifelse(which(is.na(off_3)) == length(off_3), dataset[length(dataset$Controller_Time_GMT), "Controller_Time_GMT"], on_3[which(is.na(off_3))+1])
  }
  len <- max(length(on_1), length(on_2), length(on_3))
  length(on_1) <- len                      
  length(off_1) <- len
  length(on_2) <- len                      
  length(off_2) <- len
  length(on_3) <- len                      
  length(off_3) <- len
  emp <- vector(mode = "list", length = len)
  shps <- c()
  pts <- cbind(emp, emp, emp, emp, emp, emp)
  if(length(light_check_group) == 1) {
    pts <- switch(light_check_group, "1" = cbind(on_1, off_1, emp, emp, emp, emp), "2" = cbind(emp, emp, on_2, off_2, emp, emp), "3" = cbind(emp, emp, emp, emp, on_3, off_3))
  }
  else if(length(light_check_group == 2)) {
    if(light_check_group[1] == "1") {
      pts <- switch(light_check_group[2], "2" = cbind(on_1, off_1, on_2, off_2, emp, emp), "3" = cbind(on_1, off_1, emp, emp, on_3, off_3))
    }
    else {
      pts <- cbind(emp, emp, on_2, off_2, on_3, off_3)
    }
  }
  else if(length(light_check_group == 3)) {
    pts <- cbind(on_1, off_1, on_2, off_2, on_3, off_3)
  }
  shade_points <- as.POSIXct(strptime(pts, '%m/%d/%Y %H:%M'))
  indices <- c(1:len, (2*len + 1):(3*len), (4*len +1):(5*len)) 
  counter <- 0
  for (i in indices) {
    if(!is.na(shade_points[i])) {
      counter <- counter + 1
      shps[[counter]] <- list(type = "rect", fillcolor = "black",line = list(color = "black"), opacity = 0.4,x0 = shade_points[i], x1 = shade_points[i + len],xref = "x", y0 = 0, y1 = 100000, yref = "y")
    }
  }
  return(shps)
}

# if the user selects to show landmarks, return a list of plotly shapes that shade in the landmark regions of a given rodent research mission
landmark_shape_server <- function(landmark_checkbox, dataset) {
  landmarks <- which(dataset$Mission_Milestone != "")
  color_palette <- c('#0090B8','#00A300','#F58B00','#E8000D','#751A55','#061F4C', '#397D02','#8E6D01')
  shapes <- list()
  shapes[[1]] <- create_shape(color_palette[1], dataset[1, "Date_1"], dataset[landmarks[1], "Date_1"])
  i <- 2
  while (i <= length(landmarks)) {
    shapes[[i]] <- create_shape(color_palette[i], dataset[landmarks[i-1], "Date_1"], dataset[landmarks[i], "Date_1"])
    i <- i+1
  } 
  shapes[[i]] <- create_shape(color_palette[i], dataset[landmarks[length(landmarks)], "Date_1"], dataset[length(dataset$Date_1), "Date_1"])
  if(landmark_checkbox) {
    return(shapes)
  }
  else {
    NULL
  }
}

# if the user selects to show landmarks, return a list of plotly shapes that shade in the landmark regions of a given rodent research mission
landmark_annotation_server <- function(landmark_checkbox, shapes, dataset, factor = "Temp_degC_ISS", dataset_radiation, increaseY = 0) {
  if(missing(dataset_radiation)) {
    dataset_radiation <- dataset
  }
  landmarks <- which(dataset$Mission_Milestone != "")
  annotations <- list()
  if(!missing(shapes) && landmark_checkbox) {
    annotations[[1]] <- create_annotation(paste("until ", gsub("_", " ", dataset[landmarks[1], "Mission_Milestone"]), sep = ""), shapes[[1]], dataset_radiation, 1, factor, increaseY)
    i <- 2
    while (i <= length(landmarks)) {
      annotations[[i]] <- create_annotation(paste("after ", gsub("_", " ", dataset[landmarks[i-1], "Mission_Milestone"]), sep = "", " until ", gsub("_", " ", dataset[landmarks[i], "Mission_Milestone"])), shapes[[i]], dataset_radiation, i, factor, increaseY)
      i <- i+1
    } 
    annotations[[i]] <- create_annotation(paste("after ", gsub("_", " ", dataset[landmarks[length(landmarks)], "Mission_Milestone"]), sep = ""), shapes[[i]], dataset_radiation, i, factor, increaseY)
    if(landmark_checkbox) {
      return(annotations)
    }
    else {
      NULL
    }
  }
}

# return a table of the summary statistics of specific columns of data, with given column names and rounded to a given number
create_summary_table <- function(data_columns, column_names, round_num) {
  if(missing(round_num)) {
    round_num <- 2
  }
  data_columns <- lapply(data_columns, function(x) {x[which(x!="")]})
  data_columns <- lapply(data_columns, function(x) {as.numeric(x)})
  stats <- matrix(nrow = 7, ncol = length(column_names))
  colnames(stats) <- column_names
  rownames(stats) <- c("mean", "standard deviation", "min", "Q1", 
                       "median", "Q3", "max")
  stats["mean", ] <- sapply(data_columns, function(x) {round(mean(x, na.rm = TRUE), round_num)})
  stats["standard deviation", ] <- sapply(data_columns, function(x) {round(sd(x, na.rm = TRUE), round_num)})
  stats["min", ] <- sapply(data_columns, function(x) {round(min(x, na.rm = TRUE), round_num)})
  stats["Q1", ] <- sapply(data_columns, function(x) {round(quantile(x, prob=0.25, na.rm = TRUE), round_num)})
  stats["median", ] <- sapply(data_columns, function(x) {round(median(x, na.rm = TRUE), round_num)})
  stats["Q3", ] <- sapply(data_columns, function(x) {round(quantile(x, prob=0.75, na.rm = TRUE), round_num)})
  stats["max", ] <- sapply(data_columns, function(x) {round(max(x, na.rm = TRUE), round_num)})
  
  return(stats)
}

# return the summary statistics of specific columns of a specific data frame, split based on given lists of indices
stats_for_column_chart <- function(data_indices, row_names, data_column_names, data) {
  stats <- matrix(nrow = length(row_names), ncol = 2)
  rownames(stats) <- row_names
  colnames(stats) <- c("mean", "standard_deviation")
  sections_inds <- length(row_names)/length(data_indices)
  sections_cols <- length(row_names)/length(data_column_names)
  indices <- list()
  data_columns <- c()
  dats <- list()
  for(i in c(1:length(data_indices))) {
    for(j in c(0:(sections_inds-1))) {
      indices[[i + j*length(data_indices)]] <- data_indices[[i]] 
    }
  }
  counter <- 1
  for(i in c(1:length(data_column_names))) {
    for(j in c(1:sections_cols)) {
      data_columns[counter] <- data_column_names[i] 
      counter <- counter+1
    }
  }
  for(i in c(1:length(indices))) {
    dats[[i]] <- data[indices[[i]],data_columns[i]] 
  } 
  stats[,"mean"] <- sapply(dats, function(x) {mean(as.numeric(x), na.rm = TRUE)})
  stats[,"standard_deviation"] <- sapply(dats, function(x) {sd(x, na.rm = TRUE)})
  
  return(stats)
}

# return a list of indices of a specific data frame based on specific start and end landmark time column values
get_inds <- function(start, end, data_column){
  if( start == "NA" ){ 
    inds <- c(1:which(data_column == end))
  } else if( end == "NA"){
    inds <- c((which(data_column == start)+1):length(data_column))
  } else {
    inds <- c((which(data_column == start)+1):which(data_column == end))
  }
  return(inds)
}

# return a list of indices of a specific radiation data frame based on specific start and end landmark time column values from a different data frame
get_inds_radiation <- function(start, end, dataset, data_column, dataset_radiation){
  if( start == "NA" ){ 
    end_point <- match(as.POSIXct(trunc(dataset[which(data_column == end), "Date_1"], "days")), dataset_radiation$Date_1)
    inds <- c(1:ifelse(is.na(end_point), 1, end_point))
  } else if( end == "NA"){ 
    start_point <- match(as.POSIXct(trunc(dataset[which(data_column == start), "Date_1"], "days")), dataset_radiation$Date_1)+1
    inds <- c(ifelse(is.na(start_point), nrow(dataset_radiation), start_point):nrow(dataset_radiation))
  } else {
    start_point <- (match(as.POSIXct(trunc(dataset[which(data_column == start), "Date_1"], "days")), dataset_radiation$Date_1)+1)
    end_point <- match(as.POSIXct(trunc(dataset[which(data_column == end), "Date_1"], "days")), dataset_radiation$Date_1)
    if(is.na(start_point) && is.na(end_point)){
      inds <- c(nrow(dataset_radiation), nrow(dataset_radiation))
    }
    else{
      inds <- c(ifelse(is.na(start_point), end_point, start_point): ifelse(is.na(end_point), start_point, end_point)) 
    }
  }
  return(inds)
}

# return a  shade shape for shading on a timeplot
create_shape <- function(fill_color, x_0, x_1) {
  shape <- list(type = "rect", fillcolor = fill_color,line = list(color = fill_color), opacity = 0.2,x0 = x_0, x1 = x_1,xref = "x", y0 = 0, y1 = 100000, yref = "y")
  return(shape)
}

# return text label for shape
create_annotation <- function(label, shape, dataset, n, factor = "Temp_degC_ISS", increaseY = 0) {
  annotation <- list(
      x = shape$x0 + difftime(as.POSIXct(shape$x1), as.POSIXct(shape$x0))/2,
      y = (n%%2)*max(as.numeric(dataset[,factor]), na.rm = TRUE) + (1-n%%2)*min(as.numeric(dataset[,factor]), na.rm = TRUE) + 0.5*(1-2*(n%%2))*(max(as.numeric(dataset[,factor]), na.rm = TRUE) - min(as.numeric(dataset[,factor]), na.rm = TRUE))/2 + increaseY*(1-2*(n%%2)),
      text = label,
      xref = "x",
      yref = 'y',
      bgcolor = "rgba(255, 255, 255, 0.8)",
      textangle = 30
    )
  return(annotation)
}

# from a list of timeplots, subplot the four timeplots into one plot
# return the new subplot with given shapes added
build_four_timeplots <- function(timeplot_list, shps, annotations) {
  timeplot_1 <- plotly_build(timeplot_list[[1]]) %>% layout(shapes = shps, autoscale = F, annotations = annotations)
  timeplot_2 <- plotly_build(timeplot_list[[2]]) %>% layout(shapes = shps)
  timeplot_3 <- plotly_build(timeplot_list[[3]]) %>% layout(shapes = shps)
  timeplot_4 <- plotly_build(timeplot_list[[4]]) %>% layout(shapes = shps) 
  timeplot <- subplot(timeplot_1, timeplot_2, timeplot_3, timeplot_4, nrows = 4, shareX = TRUE, shareY = TRUE, heights = c(0.25, 0.25, 0.25, 0.25))
  timeplot <- layout(timeplot, xaxis = list(title = "Date"),title = "Timeplot")
  return(timeplot)
}

# from a list of timeplots, subplot the three timeplots into one plot
# return the new subplot with given shapes added
build_three_timeplots <- function(timeplot_list, shps, annotations) {
  timeplot_1 <- plotly_build(timeplot_list[[1]]) %>% layout(shapes = shps, autoscale = F, annotations = annotations)
  timeplot_2 <- plotly_build(timeplot_list[[2]]) %>% layout(shapes = shps)
  timeplot_3 <- plotly_build(timeplot_list[[3]]) %>% layout(shapes = shps)
  timeplot <- subplot(timeplot_1, timeplot_2, timeplot_3, nrows = 3, shareX = TRUE, shareY = TRUE, heights = c(0.33, 0.33, 0.33))
  timeplot <- layout(timeplot, xaxis = list(title = "Date"), title = "Timeplot") 
  return(timeplot)
}

# from a list of timeplots, subplot the two timeplots into one plot
# return the new subplot with given shapes added
build_two_timeplots <- function(timeplot_list, shps, annotations) {
  timeplot_1 <- plotly_build(timeplot_list[[1]]) %>% layout(shapes = shps, autoscale = F, annotations = annotations)
  timeplot_2 <- plotly_build(timeplot_list[[2]]) %>% layout(shapes = shps)
  timeplot <- subplot(timeplot_1, timeplot_2, nrows = 2, shareX = TRUE, shareY = TRUE, heights = c(0.5, 0.5))
  timeplot <- layout(timeplot, xaxis = list(title = "Date"), title = "Timeplot")
  return(timeplot)
}

# landmark lines for comparison tab
show_comparison_landmark <- function(x, color, y) {
  shape <- list(type = "rect", fillcolor = color,line = list(color = color), opacity = 0.8,x0 = x, x1 = x,xref = "x", y0 = 0, y1 = y, yref = "y")
  return(shape)
}