
library(lubridate)
library(readxl)
library(tidyr)

#here::i_am("ProjectPowell.R")
source("AutoReadUSBRData/AutoReadUSBRData.R")
source("AutoReadCRMMS24MS.R")

# Loads in Hydrodata for storage data
hydrodata <- fReadReclamationHydroData(TRUE)


###Function that projects Lake Powell elevations/storage based on multiple factors
# that can be manipulated or left as default.
#Inputs: Inflow (MAF/yr), Release (MAF/yr), Additional Release(MAF/yr), Additional Release Duration (months), Duration (Months), Storage_Data (dataframe)
# Outputs: dataframe (storage_proj, elevation_proj, datetime)
# Default Values: Inflow = CMIP5 trace 65, Release = 6 MAF/yr, Additional Relese = 1 MAF, AR Duration = 12 months, Duration = 36 months


ProjectPowell <- function(Inflow = "Default", Release = 6, Add_Release = 1, Add_Time = 12, Duration = 36, Storage_Data)
{

  # Defines time_grid
  current_date <-Sys.Date()
  time_grid<- seq.Date(from = current_date, by = "month", length.out = Duration)

  # Defines Inflow
  if (Inflow == "Default"){
    CMIP5_inflow <- read.csv("Data/CMIP5 Hydrology Scenario.csv")
    Inflow <- data.frame(LeeFlow = CMIP5_inflow$X65* 1000000, year = CMIP5_inflow$CY)
    
    # Defines Upper Basin Use (Data wrangling)
    UBuse_raw <- read.csv("Data/UpperBasinConsumptiveUse.csv")
    UBuse_raw <- as.data.frame(t(UBuse_raw))
    UBuse_step1 <- gsub(",", "", UBuse_raw$V5)
    UBuse_raw$clean_numeric <- as.numeric(trimws(UBuse_step1))
    UBuse <- UBuse_raw[!is.na(UBuse_raw$clean_numeric) & !is.na(as.numeric(UBuse_raw$V3)), ]
    UBuse <- data.frame(year = as.numeric(UBuse$V3), total_use = UBuse$clean_numeric)
    
    # Ask Dr. Rosenberg about this, maybe do percentage of total Unreg Inflow? or minimum value?
    avg_ubuse <- mean(UBuse$total_use[UBuse$year<= 2024])
    LP_inflow <- data.frame(inflow = Inflow$LeeFlow - avg_ubuse, year = Inflow$year)
    
    # Inflow monthly proportion is based on 2021 (recent low water year)
    inflow_prop <- fMonthlyProportion("Inflow", "Lake Powell", Storage_Data, 2021)
    inflow_monthly <- merge(inflow_prop, LP_inflow)
    inflow_monthly$inflow <- inflow_monthly$prop * inflow_monthly$inflow
    
    # Projects inflow onto time grid
    inflow_i <- data.frame(datetime = time_grid)
    
    inflow_i$year  <- lubridate::year(inflow_i$datetime)
    inflow_i$month <- lubridate::month(inflow_i$datetime)
    
    inflow_i <- merge(
      inflow_i,
      inflow_monthly,
      by = c("year","month"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  else if(Inflow == "CRMMS"){
    inflow <- fAutoReadCRMMS24MS("Lake Powell", "Inflow Volume")
    inflow$datetime <- as.Date(inflow$date)
    inflow_fun <- approxfun(as.numeric(inflow$datetime),inflow$'24MS MIN PROB')
    if (length(time_grid) > length(inflow$datetime)){
      time_grid <- inflow$datetime
    }
    inflow_i <- data.frame(datetime = time_grid)
    inflow_i$inflow <- inflow_fun(as.numeric(time_grid))
    
  }
  
  

  # Defines Release
  release_prop <- fMonthlyProportion("Release volume", "Lake Powell", Storage_Data, 2021)
  release_prop$release <- release_prop$prop * Release * 1000000
  release_i <- data.frame(datetime = time_grid)
  release_i$release <- sapply(release_i$datetime,
      function(d){
          release_prop$release[month(d) == release_prop$month]
          }
  )
  
  # Defines Additional Release data frame
  # Values can be positive for upstream releases (Flaming Gorge) or negative for LP releases
  
  # Defines when additional release rule changes
  stage_end <- current_date %m+% months(cumsum(Add_Time))
  
  #  Matches releases to dates and places in data frame 
  #if no match add_release = 0
  # add_release is distributed evenly across stage
  for (i in length(Add_Release)){
    add_release_i <- data.frame(datetime = time_grid)
    add_release_i$add_release <- sapply(
      add_release_i$datetime,
      function(d) {
        stage <- which(d <= stage_end)[1]
        if(is.na(stage)){
          0
        }
        else{
          Add_Release[stage]/Add_Time[stage] * 1000000
        }
      }
  
    )
  
  # Merges inflow and add_release
  inflow_i$with_release <- inflow_i$inflow + add_release_i$add_release
  
  
  #Projects Storage and merges into single data frame
  no_add_release <- project_storage(inflow_i$inflow, release_i$release, Storage_Data, time_grid)
  with_release <- project_storage(inflow_i$with_release, release_i$release, Storage_Data, time_grid)
  
  #Adds labels to different operations
  no_add_release$label <- "No Additional Release"
  if (length(Add_Release) > 1){
    with_release$label <- "With Additional Release"
  }
  else{
    with_release$label <- paste0("Additional Release: ", Add_Release[1], " MAF in ", Add_Time[1], " months")
  }
  #Places projection into single data frame
  projection<- rbind(no_add_release, with_release)
  return(projection)
}
}


#### Function: Finds average proportion of value per month across historical Hydrodata
## Inputs: parameter ("string"), reservoir ("string")
## outputs: avg_prop (dataframe)
fAverageMonthlyProportion<- function(parameter, reservoir, hydrodata){
  
  monthly <- filter(hydrodata$dfResMonthly, ResName == reservoir & FieldName == parameter & WaterYear >= 2022)
  yearly<- filter(hydrodata$dfResAnnual, ResName == reservoir & FieldName == parameter & WaterYear >= 2022)
  combined <- merge(monthly, yearly, by = 'WaterYear')
  
  combined$prop <- combined$MonthlyValue/combined$AnnualValue
  avg_prop <- aggregate(prop ~ Month, data = combined, FUN = mean, na.rm = TRUE)
  return(avg_prop)
}



fMonthlyProportion <- function(parameter, reservoir, hydrodata, year){
  monthly <- filter(hydrodata$dfResMonthly, ResName == reservoir & FieldName == parameter & WaterYear == year)
  annual_value <- hydrodata$dfResAnnual$AnnualValue[hydrodata$dfResAnnual$ResName == reservoir & hydrodata$dfResAnnual$FieldName == parameter & hydrodata$dfResAnnual$WaterYear == year]
  monthly_prop <- data.frame(month = monthly$Month, prop = monthly$MonthlyValue / annual_value)
  return(monthly_prop)
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
  storage_data <- filter(Storage_Data$dfResDaily, FieldName == "Storage")
  storage <- numeric(length(time_grid))
  storage[1] <- storage_data$Value[which.max(storage_data$DateValue)]
  
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
  # Load in Bathymytry data
  bathy<-ReadBathymetryCritialElevations()
  elev <- sapply(storage, 
                 function(x){bathy$dfPowellBathymetry$`ELEVATION (feet)`[which.min(abs(x -bathy$dfPowellBathymetry$`Active Storage (acre-feet)`))]})
  

  # Return projection data frame
  data.frame(datetime = time_grid, storage = storage, elevation = elev, inflow = inflow)
  
}

#### Function: Plots projection onto graph
#Inputs: projection(dataframe)
#outputs: image

fplotprojection<- function(projection){
#Sets key elevations for graph
#bathy$dfPowellBathymetry$`ELEVATION (feet)`[which.min(abs(2000000 - bathy$dfPowellBathymetry$`Active Storage (acre-feet)`))]
  key_elevations <- data.frame(elevation_label = 
                               c(3490, 3514, 3525,3446, 3473, 3496, 3533, 3549), 
                             label = c( "Top of Penstocks: 3490 ft, 3.7 MAF","Vortices Elevation: 3514 ft, 4.9 MAF ", "DROA Target Elevation: 3525 ft, 5.5 MAF","2", "3","4", "6", "7"))
# Sets colors for graph
  labels <- unique(projection$label)
  release_labels <- setdiff(labels, "No Additional Release")
  release_palette <- c("darkolivegreen","darkolivegreen3", "darkolivegreen1" )
  color_values <- c("No Additional Release" = rgb (.88, .09, .79), 
                  setNames(release_palette[seq_along(release_labels)], release_labels ))
# Plots projection onto plot

  ggplot(projection,
       aes(x = datetime, y = elevation, color = label)) +
    geom_line(linewidth = 1.2) +
  
    scale_color_manual(name = "Operation Strategy", 
      values = color_values) +
  
    labs(title = "Lake Powell Elevation Projection", x = 
         "Date", y = "Elevation(ft.)")+
  
  # Plots Right side axis
    geom_hline(yintercept = c(3525, 3514), color = "red", 
             linetype = "dashed") +
    scale_y_continuous(name = "Elevation (ft.)",
            sec.axis = sec_axis(~.*1, breaks = key_elevations$elevation_label,
                  labels = key_elevations$label, name = " Active Storage(MAF)" 
                     ))
}
#### Function that takes a parameter on a time grid and sums that parameter for each year
# Inputs: df(dataframe with value and date column), parameter (string)
# Outputs: annual_value(dataframe)
fAnnualValues <- function(df, parameter){
  annual_value <- aggregate(
    df[[parameter]],
    by = list(year = format(df$datetime, "%Y")),
    FUN = sum,
    na.rm = TRUE
  )
  
  names(annual_value)[2] <- "value"
  annual_value$year <- as.integer(annual_value$year)
  
  annual_value
}
 

# Test Projections
projection <- ProjectPowell(Inflow = "Default", Release = 6, Add_Release = 1, Add_Time = 12, Duration = 36, hydrodata)
fplotprojection(projection)
