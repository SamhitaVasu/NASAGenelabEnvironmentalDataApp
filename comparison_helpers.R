# Libraries and external file used
library(shiny)
library(plotly)
library(DT)
library(R.utils)
source("helpers.R")

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

# server for displaying timeplots when comparing between datasets
comparison_timeplot_server <- function(factor, rows_selected, show_landmarks, after_transfer) {
  color_palette <- c('#0090B8','#00A300','#F58B00','#E8000D','#751A55','#061F4C', '#397D02','#8E6D01')
  i <- 1
  max_rows <- 0
  while(i <= length(rows_selected)) {
    max_rows <- ifelse(nrow(get(rows_selected[[i]]))>max_rows, nrow(get(rows_selected[[i]])), max_rows)
    i <- i +1
  }
  Day_number <- c(1:max_rows)*5/60/24
  dats <- data.frame(get(rows_selected[[1]])[,factor])
  colnames(dats) <- c("data")
  dats[1:length(dats$data), "mission"] <- rows_selected[[1]]
  landmarks <- which(get(rows_selected[[1]])$Mission_Milestone != "")
  dats[landmarks, "Landmarks"] <- get(rows_selected[[1]])[landmarks, "Mission_Milestone"]
  if(after_transfer == "TRUE") {
    dats <- dats[which(grepl("transfer", get(rows_selected[[1]])$Mission_Milestone) == TRUE):length(dats$data),]
  }
  dats$days <- Day_number[1:length(dats$data)]
  i <- 2
  j <- length(dats$data)
  while(i <= length(rows_selected)) {
    dats_2 <- data.frame(get(rows_selected[[i]])[,factor])
    colnames(dats_2) <- c("data")
    dats_2[1:length(dats_2$data), "mission"] <- rows_selected[[i]]
    landmarks <- which(get(rows_selected[[i]])$Mission_Milestone != "")
    dats_2[landmarks, "Landmarks"] <- get(rows_selected[[i]])[landmarks, "Mission_Milestone"]
    if(after_transfer == "TRUE") {
      dats_2 <- dats_2[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE):length(dats_2$data),]
    }
    dats_2$days <- Day_number[1:length(dats_2$data)]
    dats <- rbind(dats, dats_2)
    i <- i +1
    j <- length(dats$data)
  }
  y_title <- switch(factor, "Temp_degC_ISS" = "Spaceflight Temperature (Celsius)", "Temp_degC_Ground" = "Ground Control Temperature (Celsius)", "RH_percent_ISS" = "Spaceflight Relative Humidity (%)", "RH_percent_Ground" = "Ground Control Relative Humidity (%)","CO2_ppm_ISS" = "Spaceflight CO2 (ppm)", "CO2_ppm_Ground" = "Ground Control CO2 (ppm)")
  title <- switch(factor, "Temp_degC_ISS" = "Spaceflight Temperature over time", "Temp_degC_Ground" = "Ground Control Temperature over time", "RH_percent_ISS" = "Spaceflight Relative Humidity over time", "RH_percent_Ground" = "Ground Control Relative Humidity over time","CO2_ppm_ISS" = "Spaceflight CO2 over time", "CO2_ppm_Ground" = "Ground Control CO2 over time")
  timeplot <- plot_ly(x = ~dats$days, y = ~dats$data, type = 'scatter', mode = 'line', color = ~dats$mission, colors = color_palette[1:length(rows_selected)])
  timeplot <- layout(timeplot, xaxis = list(title = "Day Number since beginning of mission"),yaxis = list(title = y_title), title = title)
  if(show_landmarks) {
    line <- list(type = "line",xref = "x",yref = "y", opacity = 0.6)
    landmarks <- which(dats$Landmarks != "")
    lines <- list()
    i <- 1
    while(i <= length(rows_selected))
    {
      landmarks_subset <- landmarks[which(dats[landmarks, "mission"] == rows_selected[[i]])]
      clr <- ifelse(i%%8 == 0, color_palette[8], color_palette[i%%8])
      for (j in c(1:length(landmarks_subset))) {
        line[["x0"]] <- dats[landmarks_subset[j], "days"]
        line[["line"]] <- list(color = clr, dash = "dot")
        line[["x1"]] <- dats[landmarks_subset[j], "days"]
        line[["y0"]] <- min(as.numeric(dats$data), na.rm = TRUE)
        line[["y1"]] <- max(as.numeric(dats$data), na.rm = TRUE)
        lines <- c(lines, list(line))
      }
      i <- i + 1
    }
    timeplot <- layout(timeplot, shapes = lines)
  }
  return(timeplot)
}

# server for displaying timeplots for radiation when comparing between datasets
comparison_timeplot_server_radiation <- function(factor, rows_selected, show_landmarks, after_transfer) {
  color_palette <- c('#0090B8','#00A300','#F58B00','#E8000D','#751A55','#061F4C', '#397D02','#8E6D01')
  i <- 1
  max_rows <- 0
  rows <- c()
  first <- TRUE
  while(i <= length(rows_selected)) {
    if(exists(paste(rows_selected[[i]], "Rad", sep = ""))) {
      max_rows <- ifelse(nrow(get(paste(rows_selected[[i]], "Rad", sep = "")))>max_rows, nrow(get(paste(rows_selected[[i]], "Rad", sep = ""))), max_rows)
      if(first) {
        dats <- data.frame(get(paste(rows_selected[[i]], "Rad", sep = ""))[,factor])
        colnames(dats) <- c("data")
        dats[1:length(dats$data), "mission"] <- rows_selected[[i]]
        j <- i
        first <- FALSE
        rows <- append(rows,rows_selected[[i]])
      }
    }
    i <- i +1
  }
  Day_number <- c(1:max_rows)
  landmark_days <- match(as.POSIXct(trunc(get(rows_selected[[1]])[which(get(rows_selected[[1]])$Mission_Milestone != ""), "Date_1"], "days")), as.POSIXct(get(paste(rows_selected[[1]], "Rad", sep = ""))$Date_1))
  landmark_days <- landmark_days[!is.na(landmark_days)]
  dats[landmark_days, "Landmarks"] <- get(paste(rows_selected[[1]], "Rad", sep = ""))[landmark_days, "Mission_Milestone"]
  if(after_transfer == "TRUE") {
    dats <- dats[which(grepl("transfer", dats$Landmarks) == TRUE):length(dats$data),]
  }
  dats$days <- Day_number[1:length(dats$data)]
  i <- j + 1
  j <- length(dats$data)
  while(i <= length(rows_selected)) {
    if(exists(paste(rows_selected[[i]], "Rad", sep = ""))) {
      dats_2 <- data.frame(get(paste(rows_selected[[i]], "Rad", sep = ""))[,factor])
      colnames(dats_2) <- c("data")
      dats_2[1:length(dats_2$data), "mission"] <- rows_selected[[i]]
      landmark_days <- match(as.POSIXct(trunc(get(rows_selected[[i]])[which(get(rows_selected[[i]])$Mission_Milestone != ""), "Date_1"], "days")), as.POSIXct(get(paste(rows_selected[[i]], "Rad", sep = ""))$Date_1))
      dats_2[landmark_days, "Landmarks"] <- get(rows_selected[[i]])[which(get(rows_selected[[i]])$Mission_Milestone != ""), "Mission_Milestone"]
      if(after_transfer == "TRUE") {
        dats_2 <- dats_2[which(grepl("transfer", dats_2$Landmarks) == TRUE):length(dats_2$data),]
      }
      dats_2$days <- Day_number[1:length(dats_2$data)]
      dats <- rbind(dats, dats_2) 
      rows <- append(rows,rows_selected[[i]])
    }
    i <- i +1
    j <- length(dats$data)
  }
  y_title <- switch(factor, "Accumulation" = "Accumulated Radiation (mGy)", "GCR_Dose_mGy_d" = "GCR Daily Dose (mGy/Day)", "SAA_Dose_mGy_d" = "SAA Daily Dose (mGy/Day)", "Total_Dose_mGy_d" = "Total Daily Dose (mGy/Day)")
  title <- switch(factor, "Accumulation" = "Accumulated Radiation over time", "GCR_Dose_mGy_d" = "GCR Daily Dose (mGy/Day)", "SAA_Dose_mGy_d" = "SAA Daily Dose (mGy/Day)", "Total_Dose_mGy_d" = "Total Daily Dose (mGy/Day)")
  timeplot <- plot_ly(x = ~dats$days, y = ~dats$data, type = 'scatter', mode = 'line', color = ~dats$mission, colors = color_palette[1:length(rows)])
  timeplot <- layout(timeplot, xaxis = list(title = "Day Number since beginning of mission"),yaxis = list(title = y_title), title = title)
  if(show_landmarks) {
    line <- list(type = "line",xref = "x",yref = "y", opacity = 0.6)
    landmarks <- which(dats$Landmarks != "")
    lines <- list()
    relevant_rows <- rows_selected[which(sapply(rows_selected, function(x) {exists(paste(x, "Rad", sep = ""))}))]
    i <- 1
    while(i <= length(relevant_rows))
    {
      landmarks_subset <- landmarks[which(dats[landmarks, "mission"] == relevant_rows[i])]
      clr <- ifelse(i%%8 == 0, color_palette[8], color_palette[i%%8])
      for (j in c(1:length(landmarks_subset))) {
        line[["x0"]] <- dats[landmarks_subset[j], "days"]
        line[["line"]] <- list(color = clr, dash = "dot")
        line[["x1"]] <- dats[landmarks_subset[j], "days"]
        line[["y0"]] <- min(as.numeric(dats$data), na.rm = TRUE)
        line[["y1"]] <- max(as.numeric(dats$data), na.rm = TRUE)
        lines <- c(lines, list(line))
      }
      i <- i + 1
    }
    timeplot <- layout(timeplot, shapes = lines)
  }
  return(timeplot)
}

# server for displaying boxplots when comparing between datasets
comparison_boxplot_server <- function(factor, rows_selected, after_transfer) {
  color_palette <- c('#0090B8','#00A300','#F58B00','#E8000D','#751A55','#061F4C', '#397D02','#8E6D01')
  dats <- data.frame(get(rows_selected[[1]])[,factor])
  colnames(dats) <- c("data")
  dats[1:length(dats$data), "mission"] <- rows_selected[[1]]
  if(after_transfer == "TRUE") {
    dats <- dats[which(grepl("transfer", get(rows_selected[[1]])$Mission_Milestone) == TRUE):length(dats$data),]
  }
  i <- 2
  j <- length(dats$data)
  while(i <= length(rows_selected)) {
    dats_2 <- data.frame(get(rows_selected[[i]])[,factor])
    colnames(dats_2) <- c("data")
    dats_2[1:length(dats_2$data), "mission"] <- rows_selected[[i]]
    if(after_transfer == "TRUE") {
      dats_2 <- dats_2[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE):length(dats_2$data),]
    }
    dats <- rbind(dats, dats_2)
    i <- i +1
    j <- length(dats$data)
  }
  y_title <- switch(factor, "Temp_degC_ISS" = "Spaceflight Temperature (Celsius)", "Temp_degC_Ground" = "Ground Control Temperature (Celsius)", "RH_percent_ISS" = "Spaceflight Relative Humidity (%)", "RH_percent_Ground" = "Ground Control Relative Humidity (%)","CO2_ppm_ISS" = "Spaceflight CO2 (ppm)", "CO2_ppm_Ground" = "Ground Control CO2 (ppm)")
  title <- switch(factor, "Temp_degC_ISS" = "Spaceflight Temperature Boxplot", "Temp_degC_Ground" = "Ground Control Temperature Boxplot", "RH_percent_ISS" = "Spaceflight Relative Humidity Boxplot", "RH_percent_Ground" = "Ground Control Relative Humidity Boxplot","CO2_ppm_ISS" = "Spaceflight CO2 Boxplot", "CO2_ppm_Ground" = "Ground Control CO2 Boxplot")
  box_plot <- plot_ly(y = ~dats$data, type = "box", x = ~dats$mission, color = ~dats$mission, colors = color_palette[1:length(rows_selected)])
  box_plot <- layout(box_plot, yaxis = list(title = y_title), xaxis = list(title = "Missions"), title = title)
  return(box_plot)
}

# server for displaying boxplots for radiation when comparing between datasets
comparison_boxplot_server_radiation <- function(factor, rows_selected, after_transfer) {
  color_palette <- c('#0090B8','#00A300','#F58B00','#E8000D','#751A55','#061F4C', '#397D02','#8E6D01')
  i <- 1
  max_rows <- 0
  rows <- c()
  first <- TRUE
  while(i <= length(rows_selected)) {
    if(exists(paste(rows_selected[[i]], "Rad", sep = "")) && first) {
        dats <- data.frame(get(paste(rows_selected[[i]], "Rad", sep = ""))[,factor])
        colnames(dats) <- c("data")
        dats[1:length(dats$data), "mission"] <- rows_selected[[i]]
        j <- i
        first <- FALSE
        rows <- append(rows,rows_selected[[i]])
    }
    i <- i +1
  }
  if(after_transfer == "TRUE") {
    dats <- dats[match(as.POSIXct(trunc(get(rows_selected[[1]])[which(grepl("transfer", get(rows_selected[[1]])$Mission_Milestone) == TRUE), "Date_1"], "days")), as.POSIXct(get(paste(rows_selected[[1]], "Rad", sep = ""))$Date_1)):length(dats$data),]
  }
  i <- j + 1
  j <- length(dats$data)
  while(i <= length(rows_selected)) {
    if(exists(paste(rows_selected[[i]], "Rad", sep = ""))) {
      dats_2 <- data.frame(get(paste(rows_selected[[i]], "Rad", sep = ""))[,factor])
      colnames(dats_2) <- c("data")
      dats_2[1:length(dats_2$data), "mission"] <- rows_selected[[i]]
      if(after_transfer == "TRUE") {
        dats_2 <- dats_2[match(as.POSIXct(trunc(get(rows_selected[[i]])[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE), "Date_1"], "days")), as.POSIXct(get(paste(rows_selected[[i]], "Rad", sep = ""))$Date_1)):length(dats_2$data),]
      }
      dats <- rbind(dats, dats_2) 
      rows <- append(rows,rows_selected[[i]])
    }
    i <- i +1
    j <- length(dats$data)
  }
  y_title <- switch(factor, "Accumulation" = "Accumulated Radiation (mGy)", "GCR_Dose_mGy_d" = "GCR Daily Dose (mGy/Day)", "SAA_Dose_mGy_d" = "SAA Daily Dose (mGy/Day)", "Total_Dose_mGy_d" = "Total Daily Dose (mGy/Day)")
  title <- switch(factor, "Accumulation" = "Accumulated Radiation Boxplot", "GCR_Dose_mGy_d" = "GCR Daily Dose Boxplot", "SAA_Dose_mGy_d" = "SAA Daily Dose Boxplot", "Total_Dose_mGy_d" = "Total Daily Dose Boxplot")
  box_plot <- plot_ly(y = ~dats$data, type = "box", color = ~dats$mission, colors = color_palette[1:length(rows)])
  box_plot <- layout(box_plot, yaxis = list(title = y_title), title = title)
  return(box_plot)
}

# server for displaying boxplots when comparing between datasets
comparison_columnplot_server <- function(factor, factor_ground, rows_selected, after_transfer) {
  color_palette <- c('#0090B8','#00A300','#F58B00','#E8000D','#751A55','#061F4C', '#397D02','#8E6D01')
  color_palette_ground <- c("#005066","#005200","#a35c00","#a30008","#320b24","#020813","#1c3c01","#3d2f00")
  if(after_transfer == "TRUE") {
    means <- list(mean(as.numeric(get(rows_selected[[1]])[which(grepl("transfer", get(rows_selected[[1]])$Mission_Milestone) == TRUE):length(get(rows_selected[[1]])),factor]), na.rm = TRUE))
    sds <- list(sd(as.numeric(get(rows_selected[[1]])[which(grepl("transfer", get(rows_selected[[1]])$Mission_Milestone) == TRUE):length(get(rows_selected[[1]])),factor]), na.rm = TRUE))
    means_ground <- list(mean(as.numeric(get(rows_selected[[1]])[which(grepl("transfer", get(rows_selected[[1]])$Mission_Milestone) == TRUE):length(get(rows_selected[[1]])),factor_ground]), na.rm = TRUE))
    sds_ground <- list(sd(as.numeric(get(rows_selected[[1]])[which(grepl("transfer", get(rows_selected[[1]])$Mission_Milestone) == TRUE):length(get(rows_selected[[1]])),factor_ground]), na.rm = TRUE))
  }
  else {
    means <- list(mean(as.numeric(get(rows_selected[[1]])[,factor]), na.rm = TRUE))
    sds <- list(sd(as.numeric(get(rows_selected[[1]])[,factor]), na.rm = TRUE))
    means_ground <- list(mean(as.numeric(get(rows_selected[[1]])[,factor_ground]), na.rm = TRUE))
    sds_ground <- list(sd(as.numeric(get(rows_selected[[1]])[,factor_ground]), na.rm = TRUE))
  }
  i <- 2
  while(i <= length(rows_selected)) {
    if(after_transfer == "TRUE") {
      means <- append(means, mean <- mean(as.numeric(get(rows_selected[[i]])[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE):length(get(rows_selected[[i]])),factor]), na.rm = TRUE))
      sds <- append(sds, sd(as.numeric(get(rows_selected[[i]])[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE):length(get(rows_selected[[i]])),factor]), na.rm = TRUE))
      means_ground <- append(means_ground, mean <- mean(as.numeric(get(rows_selected[[i]])[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE):length(get(rows_selected[[i]])),factor_ground]), na.rm = TRUE))
      sds_ground <- append(sds_ground, sd(as.numeric(get(rows_selected[[i]])[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE):length(get(rows_selected[[i]])),factor_ground]), na.rm = TRUE))
    }
    else {
      means <- append(means, mean <- mean(as.numeric(get(rows_selected[[i]])[,factor]), na.rm = TRUE))
      sds <- append(sds, sd(as.numeric(get(rows_selected[[i]])[,factor]), na.rm = TRUE))
      means_ground <- append(means_ground, mean <- mean(as.numeric(get(rows_selected[[i]])[,factor_ground]), na.rm = TRUE))
      sds_ground <- append(sds_ground, sd(as.numeric(get(rows_selected[[i]])[,factor_ground]), na.rm = TRUE))
    }
    i <- i +1
  }
  y_title <- switch(factor, "Temp_degC_ISS" = "Temperature (Celsius)", "RH_percent_ISS" = "Relative Humidity (%)", "CO2_ppm_ISS" = "CO2 (ppm)")
  title <- switch(factor, "Temp_degC_ISS" = "Temperature Bar Chart","RH_percent_ISS" = "Relative Humidity Bar Chart", "CO2_ppm_ISS" = "CO2 Bar Chart")
  column_plot <- plot_ly(y = ~means, type = "bar", x = ~rows_selected, marker = list(color = color_palette), error_y = ~list(array = sds,color = '#000000'), name = "Spaceflight")
  column_plot <- column_plot %>% add_trace(y=~means_ground, marker = list(color = color_palette_ground), error_y = ~list(array = sds_ground,color = '#000000'), name = "Ground Control")
  column_plot <- layout(column_plot, yaxis = list(title = y_title), xaxis = list(title = "Missions"), title = title)
  return(column_plot)
}

# server for displaying radiation boxplots when comparing between datasets
comparison_columnplot_server_radiation <- function(factor, rows_selected, after_transfer) {
  color_palette <- c('#0090B8','#00A300','#F58B00','#E8000D','#751A55','#061F4C', '#397D02','#8E6D01')
  max_rows <- 0
  first <- TRUE
  means <- c()
  sds <- c()
  rows <- c()
  i <- 1
  n <- 1
  while(i <= length(rows_selected)) {
    if(exists(paste(rows_selected[[i]], "Rad", sep = "")) && first) {
      if(after_transfer == "TRUE") {
        means[n] <- mean(as.numeric(get(paste(rows_selected[[i]], "Rad", sep = ""))[match(as.POSIXct(trunc(get(rows_selected[[i]])[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE), "Date_1"], "days")), as.POSIXct(get(paste(rows_selected[[i]], "Rad", sep = ""))$Date_1)):length(get(paste(rows_selected[[i]], "Rad", sep = ""))),factor]), na.rm = TRUE)
        sds[n] <- sd(as.numeric(get(paste(rows_selected[[i]], "Rad", sep = ""))[match(as.POSIXct(trunc(get(rows_selected[[i]])[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE), "Date_1"], "days")), as.POSIXct(get(paste(rows_selected[[i]], "Rad", sep = ""))$Date_1)):length(get(paste(rows_selected[[i]], "Rad", sep = ""))),factor]), na.rm = TRUE)
      }
      else {
        means[n] <- mean(as.numeric(get(paste(rows_selected[[i]], "Rad", sep = ""))[,factor]), na.rm = TRUE)
        sds[n] <- sd(as.numeric(get(paste(rows_selected[[i]], "Rad", sep = ""))[,factor]), na.rm = TRUE) 
      }
      rows[n] <- rows_selected[[i]]
      n <- n +1
      j <- i
      first <- FALSE
    }
    i <- i +1
  }
  i <- j + 1
  while(i <= length(rows_selected)) {
    if(exists(paste(rows_selected[[i]], "Rad", sep = ""))) {
      if(after_transfer == "TRUE") {
        means[n] <- mean(as.numeric(get(paste(rows_selected[[i]], "Rad", sep = ""))[match(as.POSIXct(trunc(get(rows_selected[[i]])[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE), "Date_1"], "days")), as.POSIXct(get(paste(rows_selected[[i]], "Rad", sep = ""))$Date_1)):length(get(paste(rows_selected[[i]], "Rad", sep = ""))),factor]), na.rm = TRUE)
        sds[n] <- sd(as.numeric(get(paste(rows_selected[[i]], "Rad", sep = ""))[match(as.POSIXct(trunc(get(rows_selected[[i]])[which(grepl("transfer", get(rows_selected[[i]])$Mission_Milestone) == TRUE), "Date_1"], "days")), as.POSIXct(get(paste(rows_selected[[i]], "Rad", sep = ""))$Date_1)):length(get(paste(rows_selected[[i]], "Rad", sep = ""))),factor]), na.rm = TRUE)
      }
      else {
        means[n] <- mean(as.numeric(get(paste(rows_selected[[i]], "Rad", sep = ""))[,factor]), na.rm = TRUE)
        sds[n] <- sd(as.numeric(get(paste(rows_selected[[i]], "Rad", sep = ""))[,factor]), na.rm = TRUE) 
      }
      rows[n] <- rows_selected[[i]]
      n <- n + 1
    }
    i <- i +1
  }
  y_title <- switch(factor, "GCR_Dose_mGy_d" = "GCR Daily Dose (mGy/Day)", "SAA_Dose_mGy_d" = "SAA Daily Dose (mGy/Day)", "Total_Dose_mGy_d" = "Total Daily Dose (mGy/Day)")
  title <- switch(factor, "GCR_Dose_mGy_d" = "GCR Daily Dose Bar Chart", "SAA_Dose_mGy_d" = "SAA Daily Dose Bar Chart", "Total_Dose_mGy_d" = "Total Daily Dose Bar Chart")
  column_plot <- plot_ly(y = ~means, type = "bar", x = ~rows, marker = list(color = color_palette), error_y = ~list(array = sds,color = '#000000'))
  column_plot <- layout(column_plot, yaxis = list(title = y_title), xaxis = list(title = "Missions"), title = title)
  return(column_plot)
}

# check if a factor is selected
if_comparison_factor_selected <- function(chosen_factors, factor) {
  bool <- FALSE
  if(length(chosen_factors) && chosen_factors == factor){
    bool <- TRUE
  }
  else {
    i <- 1
    while(i <= length(chosen_factors)){
      bool <- ifelse(chosen_factors[i] == factor, TRUE, bool)
      i <- i+1
    }
  }
  return(bool)
}