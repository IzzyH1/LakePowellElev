
library(lubridate)

here::i_am("ProjectPowell.R")
source("AutoReadUSBRData/AutoReadUSBRData.R")
source("AutoReadCRMMS24MS.R")

# Loads in Hydrodata for storage data
hydrodata <- fReadReclamationHydroData(TRUE)

projection <- ProjectPowell(Inflow = "Default", Release = 6, Add_Release = c(1,0), Add_Time = 12, Duration = 36, hydrodata)


###Function that projects Lake Powell elevations/storage based on multiple factors
# that can be manipulated or left as default.
#Inputs: Inflow (MAF/yr), Release (MAF/yr), Additional Release(MAF/yr), Additional Release Duration (months), Duration (Months), Current_Storage (MAF)
# Outputs: dataframe (storage_proj, elevation_proj, datetime)
# Default Values: Inflow = CRMMS Min Prob, Release = 6 MAF/yr, Additional Relese = 1, AR Duration = 12, Duration = 24 months


ProjectPowell <- function(Inflow = "Default", Release = 6, Add_Release = c(1, 0), Add_Time = 12, Duration = 36, Storage_Data)
{

  # Defines time_grid
  current_date <-Sys.Date()
  time_grid<- seq.Date(from = current_date, by = "month", length.out = Duration)

  # Defines Inflow
  if (Inflow == "Default"){
    CMIP5_inflow <- read.csv("Data/CMIP5 Hydrology Scenario.csv")
    Inflow <- data.frame(inflow = CMIP5_inflow$X65 * 1000000 , year = CMIP5_inflow$CY)
    
    # Adjusts Lee's Ferry Natural Flow to Lake Powell Inflow
    historic_LP_flow <- data.frame(LP_inflow = hydrodata$dfResAnnual$AnnualValue[hydrodata$dfResAnnual$ResName == "Lake Powell" & hydrodata$dfResAnnual$FieldName == "Inflow Volume"], year = hydrodata$dfResAnnual$WaterYear[hydrodata$dfResAnnual$ResName == "Lake Powell"& hydrodata$dfResAnnual$FieldName == "Inflow Volume"])
    historic_Lee_flow <- data.frame(LeeFlow = Inflow$inflow[Inflow$year >= 2000], year = Inflow$year[Inflow$year >= 2000])
    Lee2Powell_df <- merge(historic_Lee_flow, historic_LP_flow, by = "year")
    Lee2Powell <- lm(Lee2Powell_df$LeeFlow ~ Lee2Powell_df$LP_inflow)
  }
  
  inflow_prop <- fAverageMonthlyProportion("Inflow", "Lake Powell", hydrodata)
  inflow_monthly <- merge(inflow_prop, Inflow)
  inflow_monthly$inflow <- inflow_monthly$prop * inflow_monthly$inflow
  
  
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
  release_prop <- fAverageMonthlyProportion("Release volume", "Lake Powell", hydrodata)
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
  no_add_release <- project_storage(inflow_i$inflow, release_i$release, hydrodata, time_grid)
  with_release <- project_storage(inflow_i$with_release, release_i$release, hydrodata, time_grid)
  projection<- data.frame(no_release_storage = no_add_release$storage, no_release_elevation = no_add_release$elevation,
                          with_release_storage = with_release$storage, with_release_elevation = with_release$elevation, Date = no_add_release$datetime)
  return(projection)
}




#### Function: Finds average proportion of value per month across historical Hydrodata
## Inputs: parameter ("string"), reservoir ("string")
## outputs: avg_prop (dataframe)
fAverageMonthlyProportion<- function(parameter, reservoir, hydrodata){
  
  monthly <- filter(hydrodata$dfResMonthly, ResName == reservoir & FieldName == parameter & WaterYear == 2022)
  yearly<- filter(hydrodata$dfResAnnual, ResName == reservoir & FieldName == parameter & WaterYear == 2022)
  combined <- merge(monthly, yearly, by = 'WaterYear')
  
  combined$prop <- combined$MonthlyValue/combined$AnnualValue
  avg_prop <- aggregate(prop ~ Month, data = combined, FUN = mean, na.rm = TRUE)
  return(avg_prop)
}



# Storage Projection Function:
# Inputs: Inflow, Outflow (Vectors)
#         Storage data (data frame)
#         time_grid (List)
# Output: Data Frame - datetime, storage, elevatrion, label, scenario 
# Storage = Previous Storage + Inflow - Outflow - Evaporation
# Evaporation per timestep = Evaporation per year/365*days in time step
project_storage <- function(inflow, outflow, Storage_Data, time_grid)
{
  Storage_Data <- filter(Storage_Data$dfResDaily, FieldName == "Storage")
  storage <- numeric(length(time_grid))
  storage[1] <- Storage_Data$Value[which.max(Storage_Data$DateValue)]
  
  # Set Evaporation
  # Evaporation is in acre feet per year
  # Define Evaporation Approximation
  #evap_data <- read.csv("Data/dfPowellEvap.csv")
  #evap_fun <- approxfun(evap_data$Total.Storage..ac.ft.,     
  #evap_data$EvapVolMaxLo/12)
  evap<- filter(hydrodata$dfResDaily, FieldName == "Evaporation", ResName == "Lake Powell" )
  evap_df <- merge(evap, Storage_Data, by = "DateValue")
  evap_fun <- approxfun(evap_df$Value.y, evap_df$Value.x)
  
  
  
  
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
  
  data.frame(datetime = time_grid, storage = storage, elevation = elev)
  
}

