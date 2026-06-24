
library(lubridate)

here::i_am("ProjectPowell.R")
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
    Inflow <- data.frame(LeeFlow = CMIP5_inflow$X65, year = CMIP5_inflow$CY)
    
    # Adjusts Lee's Ferry Natural Flow to Lake Powell Inflow Volume
    historic_LP_flow <- data.frame(LP_inflow = Storage_Data$dfResAnnual$AnnualValue[Storage_Data$dfResAnnual$ResName == "Lake Powell" & Storage_Data$dfResAnnual$FieldName == "Inflow Volume"], year = Storage_Data$dfResAnnual$WaterYear[Storage_Data$dfResAnnual$ResName == "Lake Powell"& Storage_Data$dfResAnnual$FieldName == "Inflow Volume"])
    #historic_Lee_flow <- read.csv()
    
    historic_Lee_flow <- data.frame(LeeFlow = Inflow$LeeFlow[Inflow$year >= 2000], year = Inflow$year[Inflow$year >= 2000])
    Lee2Powell_df <- merge(historic_Lee_flow, historic_LP_flow, by = "year")
    Lee2Powell_approx <- approxfun(Lee2Powell_df$LeeFlow, Lee2Powell_df$LP_inflow)
    Inflow$inflow <- Lee2Powell_approx(Inflow$LeeFlow) * 1000000
    
    inflow_prop <- fAverageMonthlyProportion("Inflow", "Lake Powell", Storage_Data)
    inflow_monthly <- merge(inflow_prop, Inflow)
    inflow_monthly$inflow <- inflow_monthly$prop * inflow_monthly$inflow
  }
  else if(Inflow == "CRMMS"){
    inflow <- fAutoReadCRMMS24MS("Lake Powell", "Inflow Volume")
    inflow$datetime <- as.Date(inflow$date)
    inflow_fun <- approxfun(as.numeric(inflow$datetime),inflow$'24MS MIN PROB')
    inflow_i <- inflow_fun(time_grid)
    
  }
  
  
  
  # Projects inflow onto time grid
  inflow_i <- data.frame(datetime = time_grid)
  
  inflow_i$year  <- lubridate::year(inflow_i$datetime)
  inflow_i$Month <- lubridate::month(inflow_i$datetime)
  
  inflow_i <- merge(
    inflow_i,
    inflow_monthly[, c("year","Month","inflow")],
    by = c("year","Month"),
    all.x = TRUE
  )

  # Defines Release
  release_prop <- fAverageMonthlyProportion("Release volume", "Lake Powell", Storage_Data)
  release_monthly <- release_prop$prop * Release * 1000000
  release <- cbind(release_prop, release_monthly)
  release_i <- data.frame(datetime = time_grid)
  release_i$release <- sapply(release_i$datetime,
      function(d){
          release$release_monthly[month(d) == release$Month]
          }
  )
  
  # Defines Additional Release data frame
  # Values can be positive for upstream releases (Flaming Gorge) or negative for LP releases
  
  # Defines when additional release rule changes
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
    with_release$label <- paste0("Additional Release: ", Add_Release[1], " MAF in", Add_Time[1], " months")
  }
  #Places projection into single data frame
  projection<- rbind(no_add_release, with_release)
  return(projection)
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
  data.frame(datetime = time_grid, storage = storage, elevation = elev)
  
}

projection <- ProjectPowell(Inflow = "CRMMS", Release = 6, Add_Release = 1, Add_Time = 12, Duration = 36, hydrodata)

#Sets key elevations for graph
key_elevations <- data.frame(elevation_label = 
                               c(3490, 3525, 3473, 3496, 3515, 3533), 
                             label = c( "Minimum Power Pool: 3490 ft, 3.7 MAF", "DROA Target Elevation: 3525 ft, 5.5 MAF", "3","4", "5", "6"))
# Plots projection onto plot

ggplot(projection,
       aes(x = datetime, y = elevation, color = label)) +
  geom_line(linewidth = 1.2) +
  
  scale_color_manual(name = "Operation Strategy", 
    values = c("No Additional Release" = rgb(.88,.09,.79), 
    setNames("darkolivegreen3", test_projection$label[length(test_projection$label)]))) +
  
  labs(title = "Lake Powell Elevation Projection", x = 
         "Date", y = "Elevation(ft.)")+
  
  # Plots Right side axis
  geom_hline(yintercept = c(3525, 3490), color = "red", 
             linetype = "dashed") +
  scale_y_continuous(name = "Elevation (ft.)",
            sec.axis = sec_axis(~.*1, breaks = key_elevations$elevation_label,
                  labels = key_elevations$label, name = " Active Storage(MAF)" 
                     ))



