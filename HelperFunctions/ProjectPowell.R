
library(lubridate)
library(readxl)
library(tidyr)


source("HelperFunctions/AutoReadUSBRData/AutoReadUSBRData.R")
source("HelperFunctions/AutoReadCRMMS24MS.R")

# Loads in Hydrodata for storage data
hydrodata <- fReadReclamationHydroData(FALSE)


###Function that projects Lake Powell elevations/storage based on multiple factors
# that can be manipulated or left as default.
#Inputs: Inflow (MAF/yr), Release (MAF/yr), Additional Release(MAF/yr), Additional Release Duration (months), Duration (Months), Storage_Data (dataframe)
# Outputs: dataframe (storage_proj, elevation_proj, datetime)
# Default Values: Inflow = CRMMS, Release = 6 MAF/yr, Additional Relese = 1 MAF, AR Duration = 12 months, Duration = 36 months


ProjectPowell <- function(Inflow, Release, Release_Time, Add_Release, Add_Time, Duration, Storage_Data = hydrodata)
{

  # Defines time_grid
  current_date <-Sys.Date()
  time_grid<- seq.Date(from = current_date, by = "month", length.out = Duration)

  #Defines INflow
  if(Inflow == "CRMMS"){
    inflow <- fAutoReadCRMMS24MS("Lake Powell", "Inflow Volume")
    inflow$datetime <- as.Date(inflow$date)
    inflow_fun <- approxfun(as.numeric(inflow$datetime),inflow$'24MS MIN PROB')
    if (length(time_grid) > length(inflow$datetime)){
      time_grid <- inflow$datetime[-(1:2)]   #Indexing makes time_grid start at current month
    }
    inflow_i <- data.frame(datetime = time_grid)
    inflow_i$inflow <- inflow_fun(as.numeric(time_grid))
    
  }
  
  
  # Defines when Release rule changes
  Release_Time[is.na(Release_Time)] <- 0
  stage_end <- current_date %m+% months(cumsum(Release_Time))

  # Defines Release
  release_prop <- fMonthlyProportion("Release volume", "Lake Powell", Storage_Data, 2021)
  
  for (i in seq_along(Release)){
    release_i <- data.frame(datetime = time_grid)
    release_i$release <- sapply(
      release_i$datetime,
      function(d) {
        stage <- which(d <= stage_end)[1]
        if(is.na(stage)){
          0
        }
        else{
          release_prop$prop[month(d) == release_prop$month] * Release[i]*1000000
        }
      }
    )
  
 
  # Defines Additional Release data frame
  # Values can be positive for upstream releases (Flaming Gorge) or negative for LP releases
  
  # Defines when additional release rule changes
  #req(Add_Time)
  #req((length(Add_Time) > 0),
  Add_Time[is.na(Add_Time)] <- 0
  stage_end <- current_date %m+% months(cumsum(Add_Time))
  
  #  Matches releases to dates and places in data frame 
  #if no match add_release = 0
  # add_release is distributed evenly across stage
    add_release_i <- data.frame(datetime = time_grid)
    add_release_i$add_release <- sapply(
      add_release_i$datetime,
      function(d) {
        stage <- which(d <= stage_end)[1]
          if(is.na(stage)){
            return(0)
          }
        Add_Release[i]/Add_Time[i] * 1000000
      }
    )
  
  # Merges inflow and add_release
  inflow_i$with_release <- inflow_i$inflow + add_release_i$add_release
  
  
  #Projects Storage and merges into single data frame
  no_add_release <- project_storage(inflow_i$inflow, release_i$release, Storage_Data, time_grid)
  with_release <- project_storage(inflow_i$with_release, release_i$release, Storage_Data, time_grid)
  if("crashed" %in% c(no_add_release, with_release)){
    return("crashed")
  }
  
  #Adds labels to different operations
  no_add_release$label <- "No Additional Release"
  if (length(Add_Release) > 1){
    with_release$label <- "With Additional Release"
  }
  else{
    with_release$label <- paste0("Additional Release: ", Add_Release[1], " MAF in ", Add_Time[1], " months")
  }
  
  #Places projection into single data frame
  no_add_release$inflow <- inflow_i$inflow
  with_release$inflow <- inflow_i$with_release
  projection<- rbind(no_add_release, with_release)
  return(projection)
  }

}


### Function: finds monthly proportion of value for a specific parameter and a specific year
fMonthlyProportion <- function(parameter, reservoir, hydrodata, year){
  monthly <- filter(hydrodata$dfResMonthly, ResName == reservoir & FieldName == parameter & WaterYear == year)
  annual_value <- hydrodata$dfResAnnual$AnnualValue[hydrodata$dfResAnnual$ResName == reservoir & hydrodata$dfResAnnual$FieldName == parameter & hydrodata$dfResAnnual$WaterYear == year]
  monthly_prop <- data.frame(month = monthly$Month, prop = monthly$MonthlyValue / annual_value)
  return(monthly_prop)
}

### Function: Takes stage length inputs and returns the date range associated with stages
# Inputs: stage_durations (list of lengths), parameter (string)
#Outputs: date_ranges (data frame)

fCalculateDateRange <- function(stage_durations, parameter){
  start_date <- as.Date(rep(NA, length(stage_durations)))
  start_date[1] <- floor_date(Sys.Date(), unit = "month")
  end_date <- as.Date(rep(NA, length(stage_durations)))
  stage_number <- numeric(length(stage_durations))
  text <- as.character(rep(NA, length(stage_durations)))

  for (i in seq_along(stage_durations)){
    end_date[i] <- start_date[i] %m+% months(stage_durations[i])
    start_date[i + 1] <- end_date[i]
    stage_number[i] <- i
  }

  for (i in seq_along(stage_number)){
    text[i] <- paste0(parameter, " Stage ",stage_number[i], ": ", start_date[i], " - ", end_date[i])
  if (stage_durations[i] == 0){
    text[i] <- NA
  }
  }  
  
  return(text)
}

# Storage Projection Function:
# Inputs: Inflow, Outflow (Vectors)
#         Storage data (data frame)
#         time_grid (List)
# Output: Data Frame - datetime, storage, elevation, label, scenario 
# Storage = Previous Storage + Inflow - Outflow - Evaporation
# Evaporation per timestep = Evaporation per year/365*days in time step
project_storage <- function(inflow, outflow, Storage_Data, time_grid)
{
  storage_data <- filter(Storage_Data$dfResDaily, FieldName == "Storage", ResName == "Lake Powell")
  storage <- numeric(length(time_grid))
  storage[1] <- storage_data$Value[which.max(as.Date(storage_data$DateValue))]
  
  # Set Evaporation
  # Evaporation is in acre feet per year
  # Define Evaporation Approximation
  evap<- filter(Storage_Data$dfResDaily, FieldName == "Evaporation", ResName == "Lake Powell" )
  evap_df <- merge(evap, storage_data, by = "DateValue")
  evap_fun <- approxfun(evap_df$Value.y, evap_df$Value.x)
  
  
  
  # Mass Balance time series
  for (i in 2:length(time_grid)) {
    evap_i <- evap_fun(storage[i-1])
    
    storage[i] <-
      storage[i - 1] +
      inflow[i] -
      outflow[i] -
      evap_i
    
  }
  
  # Returns crash message if storage is less than 0
  if (any(storage <= 0)){
    return("crashed")
  }
  
  # Load in Bathymytry data
  bathy<-ReadBathymetryCritialElevations()
  elev <- sapply(storage, 
                 function(x){bathy$dfPowellBathymetry$`ELEVATION (feet)`[which.min(abs(x -bathy$dfPowellBathymetry$`Active Storage (acre-feet)`))]})
  

  # Return projection data frame
  data.frame(datetime = time_grid, storage = storage, elevation = elev, inflow = inflow, outflow = outflow)
  
}

#### Function: Plots projection onto graph
#Inputs: projection(dataframe)
#outputs: image

fplotprojection<- function(projection, elevation_input = NULL){
#Sets key elevations for graph
#bathy$dfPowellBathymetry$`ELEVATION (feet)`[which.min(abs(2000000 - bathy$dfPowellBathymetry$`Active Storage (acre-feet)`))]
  elevation_ticks <- data.frame(elevation = c(3490, 3446, 3473, 3496, 3533, 3549), label = c("Top of Penstocks: 3490 ft, 3.7 MAF","2", "3","4", "6", "7"))
  key_elevations<- data.frame(elevation = c(3514, 3500), label =c("Vortices Elevation: 3514 ft, 4.9 MAF ", "Final EIS Critical Elevation: 3500 ft"))
  
  if (!is.null(elevation_input)){
    key_elevations <- rbind(key_elevations, elevation_input)
  }
  
  elevation_ticks <- rbind(elevation_ticks, key_elevations)
  
  # Sets colors for graph
  labels <- unique(projection$label)
  release_labels <- setdiff(labels, c("No Additional Release", "24MS MIN PROB"))
  release_palette <- c("darkolivegreen","darkolivegreen3", "darkolivegreen1" )
  color_values <- c("No Additional Release" = rgb (.88, .09, .79), "24MS MIN PROB" = rgb(.046,.037,.333),
                  setNames(release_palette[seq_along(release_labels)], release_labels ))
# Plots projection onto plot

  ggplot(projection,
       aes(x = datetime, y = elevation, color = label)) +
    geom_line(linewidth = 1.2) +
  
    scale_color_manual(name = "Operation Strategy", 
      values = color_values) +
  
    labs(title = "Lake Powell Elevation Projection", x = 
         "Date", y = "Elevation(ft.)" )+
  
  # Plots Right side axis
    geom_hline(yintercept = key_elevations$elevation, color = "red", 
             linetype = "dashed") +
    scale_y_continuous(name = "Elevation (ft.)",
            sec.axis = sec_axis(~.*1, breaks = elevation_ticks$elevation,
                  labels = elevation_ticks$label, name = " Active Storage(MAF)" )) +
    
    theme(axis.title = element_text(size = 18), 
          axis.ticks = element_line(linewidth = 1.5))  
            
}


#### Function that takes a parameter on a time grid and sums that parameter for each year
# Inputs: df(dataframe with value and date column), parameter (string)
# Outputs: annual_value(dataframe)
fAnnualValues <- function(df, parameter, Storage_Data){
  # Focuses data frames to concerned value
  df_value <- data.frame(value = df[[parameter]]/ 1000000, datetime = df$datetime)
  
  FieldName <- switch(
    parameter,
    inflow = "Inflow Volume",
    outflow = "Release volume",
    stop("Unknown parameter")
  )
  
  storage_value <- data.frame(value = Storage_Data$dfResMonthly$MonthlyValue[Storage_Data$dfResMonthly$FieldName == FieldName & Storage_Data$dfResMonthly$ResName == "Lake Powell"], 
                              datetime = as.Date(Storage_Data$dfResMonthly$Date[Storage_Data$dfResMonthly$FieldName == FieldName & Storage_Data$dfResMonthly$ResName == "Lake Powell"]))
  
  
  # adds common month column to make merging/coalescing consistent
  df_monthly <- df_value %>%
    mutate(fixed_date = floor_date(datetime, "month"))
  
  storage_monthly <- storage_value %>%
    mutate(fixed_date = floor_date(datetime, "month"))
  
  # Summing of Values by water year
  annual_value <- storage_monthly %>%
    full_join(df_monthly, by = "fixed_date", suffix = c("_actual", "_forecasted"))%>%
    dplyr::mutate(value = coalesce(value_actual, value_forecasted),
           water_year = if_else(
             lubridate::month(fixed_date) >= 10,
             lubridate::year(fixed_date) + 1,
             lubridate::year(fixed_date)
           )) %>%
    group_by(water_year)%>%
    dplyr::summarise(value = sum(value, na.rm = TRUE))%>%
    ungroup()
  
  # Changes Column Name to more readable format
  value_name <- paste0(FieldName, " (MAF)")
  annual_value <- dplyr::rename(annual_value, !!value_name := value)  
  return(annual_value)
}
 

# Test Projections
#projection <- ProjectPowell(Inflow = "CRMMS", Release = c(5,6), Release_Time = c(12, 12), Add_Release = 1, Add_Time = 12, Duration = 36, Storage_Data = hydrodata)
dates <- fCalculateDateRange(c(12,12,12), "Inflow")
#elevation_input <- data.frame(elevation = 3500, label = "3500 label")
#annual_outflow <- fAnnualValues(projection, "outflow", hydrodata)

#CRMMS_projection <- fAutoReadCRMMS24MS("Lake Powell", "Pool Elevation")
#CRMMS_filtered <- data.frame(datetime = CRMMS_projection$date, elevation = CRMMS_projection$`24MS MIN PROB`, label = "24MS MIN PROB", storage = NA, inflow = NA, outflow = NA)
#test <- rbind(projection, CRMMS_filtered)
