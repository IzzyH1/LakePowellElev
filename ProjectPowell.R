
library(lubridate)

here::i_am("ProjectPowell.R")
source("AutoReadUSBRData/AutoReadUSBRData.R")
source("AutoReadCRMMS24MS.R")




###Function that projects Lake Powell elevations/storage based on multiple factors
# that can be manipulated or left as default.
#Inputs: Inflow (MAF/yr), Release (MAF/yr), Additional Release(MAF/yr), Additional Release Duration (months), Duration (Months), Current_Storage (MAF)
# Outputs: dataframe (storage_proj, elevation_proj, datetime)
# Default Values: Inflow = CRMMS Min Prob, Release = 6 MAF/yr, Additional Relese = 1, AR Duration = 12, Duration = 24 months


ProjectPowell <- function(Inflow = "CRMMS", Release = 6, Add_Release = c(1, 0), Add_Time = 12, Duration = 36, Storage_Data)
{

  # Defines time_grid
  current_date <-Sys.Date()
  time_grid<- seq.Date(from = current_date, by = "month", length.out = Duration)
  #Figure out a way to extract the month from the Dates in the time_gridf
  # May need to change the start date to the beginning of the current month
  #time_grid <- cbind(time_grid, month)
  
  # Defines Inflow
  if (Inflow == "CRMMS"){
    # Figure out how to integrate AutoReadCRMMS Function
    #May be replacing with other inflow source, so hold off
    pass()
  }
  else {
    inflow_prop <- fAverageMonthlyProportion("Inflow", "Lake Powell")
    inflow_monthly <- inflow_prop$prop * Inflow
    inflow <- cbind(inflow_prop, inflow_monthly)
    
    # Projects inflow onto time_grid
    inflow_i <- data.frame(datetime = time_grid)
    inflow_i$inflow <- sapply(inflow_i$datetime,
          function(d){
            inflow$inflow_monthly[month(d) == inflow$Month]
          }
      
    )
    
  }
  
  # Defines Release
  release_prop <- fAverageMonthlyProportion("Release volume", "Lake Powell")
  release_monthly <- release_prop$prop * Release
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
        Add_Release[stage]/Add_Time[stage]
      }
    }
  )
  
  # Merges inflow and add_release
  inflow_i$with_release <- inflow_i$inflow + add_release_i$add_release
  
  #Projects Storage and merges into single data frame
  no_add_release <-project_storage(inflow_i$inflow, release_i$release,  )
}
#### Function: Finds average proportion of value per month across historical Hydrodata
## Inputs: parameter ("string"), reservoir ("string")
## outputs: avg_prop (dataframe)
fAverageMonthlyProportion<- function(parameter, reservoir){
  #df <- fReadReclamationHydroData(TRUE)
  monthly <- filter(df$dfResMonthly, ResName == reservoir & FieldName == parameter & WaterYear == 2022)
  yearly<- filter(df$dfResAnnual, ResName == reservoir & FieldName == parameter & WaterYear == 2022)
  combined <- merge(monthly, yearly, by = 'WaterYear')
  
  combined$prop <- combined$MonthlyValue/combined$AnnualValue
  avg_prop <- aggregate(prop ~ Month, data = combined, FUN = mean, na.rm = TRUE)
  return(avg_prop)
}

# Storage Projection Function:
# Inputs: Inflow, Outflow, Current Storage (Vectors)
#         evap_csv (file path as String)  
#         time_grid (List)
# Output: Data Frame - datetime, storage, elevatrion, label, scenario 
# Storage = Previous Storage + Inflow - Outflow - Evaporation
# Evaporation per timestep = Evaporation per year/365*days in time step
project_storage <- function(inflow, outflow, Storage_Data, time_grid)
{
  
  storage <- numeric(length(time_grid))
  storage[1] <- Storage_Data$Value[which.max(Storage_Data$DateValue)]
  
  # Set Evaporation
  # Evaporation is in acre feet per year
  # Define Evaporation Approximation
  #evap_data <- read.csv("Data/dfPowellEvap.csv")
  #evap_fun <- approxfun(evap_data$Total.Storage..ac.ft.,     
  #evap_data$EvapVolMaxLo/12)
  evap<- filter_parameter("Evaporation", "Lake Powell", hydro_data$dfResDaily)
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

