# AutoReadUSBRData.r
#
# This script auto reads USBR Lake Powell and Lake Mead data from Reclamation's HydroData Web portal
# 
# https://www.usbr.gov/uc/water/hydrodata/reservoir_data/site_map.html
#
# This script also loads Bathymetry data for Lake Powell and Lake Mead
# This script also loads critical elevations for the two reservoirs
#
# This is a beginning R-programming effort! There could be lurking bugs or basic coding errors that I am not even aware of.
# Please report bugs/feedback to me (contact info below)
#
# We auto-read all Lake Powell (Code 919) and Lake Mead (921) available data at the daily time step.
#
# This data is:
#  Lake Powell (919) and field (code)
#     Storage - 17
#     Evaporation - 25
#     Inflow - 29
#     Inflow volume - 30
#     Unregulated inflow - 33
#     Unregulated inflow volume - 34
#     Total Release - 42
#     Release volume - 43
#     Area - 89
#     Pool elevation - 49
#  Lake Mead (921)
#     Storage - 17
#     Total Release - 42
#     Pool elevation - 49
#     Release volume - 43
#
#  The auto read URL command is:
#  'https://www.usbr.gov/uc/water/hydrodata/reservoir_data/RESID/csv/FIELDID.csv'

#
# The data wrangling strategy is:
#   1. Loop through Reservoirs and Fields
#   2.   Auto download the individual field CSV file format
#   2. Combine into one data frame for each reservoir with columns as fields
#   4. Clean and covert Reclamation's date format to a format R understands
#   5. Move into Narrow format - [Date][Reservoir][Field][Units][Value]
#   6. Calculate Water Year and sum daily values to annual and 10-year running sums.

# David E. Rosenberg
# January 28, 2026
# Utah State University
# david.rosenberg@usu.edu

rm(list = ls())  #Clear history

# Load required libraries in 1 go
# List of packages
load.lib <- c("tidyverse", "readxl", "RColorBrewer", "dplyr", "expss", "reshape2", "pracma", "lubridate", "directlabels", "plyr", "stringr", "ggplot2", "ggpubr", "ggrepel", "zoo", "here")
# Then we select only the packages that aren't currently installed.
install.lib <- load.lib[!load.lib %in% installed.packages()]
# And finally we install the missing packages, including their dependency.
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
# After the installation process completes, we load all packages.
sapply(load.lib,require,character=TRUE)

###### Define a function that Reads in all the available reservoir data from Reclamations HydroData webportal.
# https://www.usbr.gov/uc/water/hydrodata/reservoir_data/site_map.html

fReadReclamationHydroData <- function(FromHydroData) {
 
    # If FromHydroData is TRUE, then read from the hydroportal. Else just load the most recent CSV files
    
    # Use here package to identify relative paths for the 3 csv files that represent all data downloaded from Reclamation's Hydrodata web portal
    fAnnualData <- here("AutoReadUSBRData", "dfResDataAnnual.csv")
    fMonthlyData <- here("AutoReadUSBRData","dfResDataMonthly.csv")
    fDailyData <- here("AutoReadUSBRData","dfResDataDaily.csv")
     
    #Print out the paths so we can check they are correct.
    print(getwd())
    print(fAnnualData)
    print(fMonthlyData)
    print(fDailyData)
 
    if(FromHydroData != TRUE ) {
      
      dfResDataAnnual <- read.csv(file = fAnnualData, header=TRUE,  sep=",")
      dfResDataMonthly <- read.csv(file = fMonthlyData, header=TRUE,  sep=",")
      dfResDataDaily <- read.csv(file = fDailyData, header=TRUE,  sep=",")
      
      #dfResDataAnnual <- read.csv(file = "dfResDataAnnual.csv", header=TRUE,  sep=",")
      #dfResDataMonthly <- read.csv(file = "dfResDataMonthly.csv", header=TRUE,  sep=",")
      #dfResDataDaily <- read.csv(file = "dfResDataDaily.csv", header=TRUE,  sep=",")
      
      # return(list(first_df = dfResDataDaily, second_df = dfResDataMonthly, third_df = dfResDataAnnual))
      return(list(dfResDaily = dfResDataDaily, dfResMonthly = dfResDataMonthly, dfResAnnual = dfResDataAnnual))
      
    }

    #We will read from the HydroDataPortal
    
    sExcelMeta <- here("AutoReadUSBRData", "USBRWebPortalMetaData.xlsx")
    
    
    dfReservoirs <- read_excel(sExcelMeta, sheet = "Reservoirs")
    dfFields <- read_excel(sExcelMeta, sheet = "Fields")
    
    
    # Create a master data frame of reservoir data
    dfResData <- data.frame(datetime = 0, Value = 0, FieldID = 0, ResID = 0)
    
    # Loop over the reservoirs and fields
    
    for (iRes in dfReservoirs$ResID) {
    #for (iRes in 921) {  
      for (iField in dfFields$FieldID) {
        
         # Create the url call
        sResFieldURL <- paste0('https://www.usbr.gov/uc/water/hydrodata/reservoir_data/', iRes, '/csv/', iField,'.csv')

        # Log the reservoir name and field for use in error checking.
        dfResNameTemp <- dfReservoirs %>% filter(ResID == iRes) %>% select(ResName)
        dfFieldNameTemp <- dfFields %>% filter(FieldID == iField) %>% select(FieldName)
        
        # print(sResFieldURL)
        
        dfTemp <- try(read.csv(file=sResFieldURL, 
                               header=TRUE,
                               
                               stringsAsFactors=FALSE,
                               sep=","))
        if (!inherits(dfTemp, "try-error")) {
          # Proceed with data processing if no error occurred
          
          print(paste("Successfully read -", dfResNameTemp$ResName[1],":", dfFieldNameTemp$FieldName[1], " - ", sResFieldURL))
          #print(paste("Successfully read - ", sResFieldURL))
                    # Change the second column from the field name to value
          colnames(dfTemp)[2] <- "Value"
          # Add reservoir and field codes as new fields
          dfTemp$FieldID <- iField
          dfTemp$ResID <- iRes
          #
          #
           dfResData <- rbind(dfResData,  dfTemp)
        } else {
          print(paste("Skipping due to error -", dfResNameTemp$ResName[1],":", dfFieldNameTemp$FieldName[1], " - ", sResFieldURL))
          
          #print(paste("Skipping", sResFieldURL, "due to an error"))
        }
      }
    }
    
    # Join in field and reservoir names and associated data
    dfResData <- left_join(dfResData, dfReservoirs, by = "ResID")
    dfResData <- left_join(dfResData, dfFields, by = "FieldID")
    
    # Remove the first row
    dfResData <- dfResData %>% slice(-1)
    
    dDateInterval <- 365 # Days
    
    #Convert date text to date value
    # dfResData$DateValue <- as.POSIXct(dfResData$datetime)
    dfResData$DateValue <- ymd(dfResData$datetime)
    
    # Convert CFS to Acre-feet per month
    nCFStoAFMon <- 60.37
    nCFStoAFDay <- nCFStoAFMon/30.5
    
    nCFSToAF <- nCFStoAFDay
    
    # Calculate Day, Month, and Year
    dfResData$Year <- year(dfResData$DateValue)
    dfResData$Month <- month(dfResData$DateValue)
    dfResData$Day <- day(dfResData$DateValue)
    
    # Calculate Water Year
    dfResData$WaterYear <- ifelse(dfResData$Month >= 10, dfResData$Year + 1, dfResData$Year)
    
    # Save the Daily data frame
    dfResDataDaily <- dfResData
    
    dfTemp <- dfResData %>% filter(Year == 2000, Month == 10, Day ==1)
    
    ### Aggregate and filter to Monthly values
    # Aggregate To Monthly values for fields with AggregateToTimePeriod == Yes
    dfFieldsAggByTimePeriod <- dfFields %>% filter(AggregateByTimePeriod == "Yes")
    
    # For fields with AggregateToTimePeroid == Yes, we aggregate
    dfTempAgg <- dfResData %>% filter(AggregateByTimePeriod == "Yes") %>% 
                              select(WaterYear, Month, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits, Value) %>%
                               group_by(WaterYear, Month, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits) %>%
                                dplyr::summarize(MonthlyValue = sum(Value))           
    
    # For fields with AggregateToTimePeriod == No, we select the first day of the month
    dfTempDay1 <- dfResData %>% filter(AggregateByTimePeriod == "No", Day == 1) %>%
                                select(WaterYear, Month, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits, Value) %>%
                                dplyr::rename(MonthlyValue = Value)
    
    # Merge back into a single Monthly data frame
    dfResDataMonthly <- rbind(dfTempAgg, dfTempDay1)
    
    ### Convert acre-feet fields to Million Acre-Feet
    dfResDataMonthly$MonthlyValue <- ifelse(dfResDataMonthly$FieldUnits == "acre-feet", dfResDataMonthly$MonthlyValue/ 1e6, dfResDataMonthly$MonthlyValue )
    dfResDataMonthly$FieldUnits <- ifelse(dfResDataMonthly$FieldUnits == "acre-feet", "million acre-feet", dfResDataMonthly$FieldUnits )

    # Calculate calendar year
    dfResDataMonthly$Year <- ifelse(dfResDataMonthly$Month >= 10, dfResDataMonthly$WaterYear - 1, dfResDataMonthly$WaterYear)
    #Calculate Date as Date Type
    dfResDataMonthly$Date <- as.Date(paste(dfResDataMonthly$Year,"-",dfResDataMonthly$Month, "-01", sep = ""), format = "%Y-%m-%d")
    
        
    
    ### Aggregate and filter to Water Year values
    # Aggregate To WaterYear values for fields with AggregateToTimePeriod == Yes
    
    dfTempAgg <- dfResData %>% filter(AggregateByTimePeriod == "Yes") %>% 
      select(WaterYear, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits, Value) %>%
      group_by(WaterYear, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits) %>%
      dplyr::summarize(AnnualValue= sum(Value))           
    
    # For fields with AggregateToTimePeriod == No, we select the first day of the Water Year (October 1)
    dfTempDay1 <- dfResData %>% filter(AggregateByTimePeriod == "No", Month == 12, Day == 1) %>%
      select(WaterYear, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits, Value) %>%
      dplyr::rename(AnnualValue = Value)
    
    # Merge back into a single data frame
    dfResDataAnnual <- rbind(dfTempAgg, dfTempDay1)
    
    ### Convert acre-feet fields to Million Acre-Feet
    dfResDataAnnual$AnnualValue <- ifelse(dfResDataAnnual$FieldUnits == "acre-feet", dfResDataAnnual$AnnualValue/ 1e6, dfResDataAnnual$AnnualValue )
    dfResDataAnnual$FieldUnits <- ifelse(dfResDataAnnual$FieldUnits == "acre-feet", "million acre-feet", dfResDataAnnual$FieldUnits )
    
    #Trim first and last years with incomplete data
    nFirstYear <- min(dfResDataAnnual$WaterYear) + 1
    nLastYear <-  max(dfResDataAnnual$WaterYear)
    
    dfResDataAnnual <- dfResDataAnnual %>% filter(WaterYear > nFirstYear, WaterYear < nLastYear)
    ###########
 
    # Write the data frames to csv files in the function local directory
    write.csv(dfResDataAnnual, fAnnualData)
    write.csv(dfResDataMonthly, fMonthlyData)
    write.csv(dfResDataDaily, fDailyData)

    return(list(dfResDaily = dfResDataDaily, dfResMonthly = dfResDataMonthly, dfResAnnual = dfResDataAnnual))
}


####### New function interp2 to return NAs for values outside interpolation range (from https://stackoverflow.com/questions/47295879/using-interp1-in-r)
interp2 <- function(x, y, xi = x, ...) {
  yi <- rep(NA, length(xi));
  sel <- which(xi >= range(x)[1] & xi <= range(x)[2]);
  yi[sel] <- interp1(x = x, y = y, xi = xi[sel], ...);
  return(yi);
}

# Example use
# interp2(xi = dfMeadEvap$`Elevation (ft)` - EvapRateToUse[1], x=dfMeadElevStor$`Elevation (ft)` , y=dfMeadElevStor$`Live Storage (ac-ft)`, method="linear")

################ Read the bathymetry and critical elevation data from Excel
ReadBathymetryCritialElevations <- function() {

    # Read elevation-storage data in from Excel
    sExcelBathymetryFile <- "Bathymetry.xlsx"
    sExcelBathymetryFile <- here("AutoReadUSBRData", sExcelBathymetryFile)
    
    print(sExcelBathymetryFile)
    
    dfMeadBathymetry <- read_excel(sExcelBathymetryFile, sheet = "Mead-Bathymetry")
    dfPowellBathymetry <- read_excel(sExcelBathymetryFile, sheet = 'Powell-Bathymetry')
    
    ################# Read the critical elevations from Excel
    sExcelElevationsFile <- "ReservoirElevationDefinitions.xlsx"
    
    sExcelElevationsFile <- here("AutoReadUSBRData", sExcelElevationsFile)
    
    print(sExcelElevationsFile)
    
    dfMeadElevations <- read_excel(sExcelElevationsFile, sheet = "MeadElevations")
    dfPowellElevations <- read_excel(sExcelElevationsFile, sheet = 'PowellElevations')
    
    # Calcualte volumes from critical elevations
    dfMeadElevations$ActiveStorageMAF <- interp2(xi = dfMeadElevations$`Elevation (feet)`, x=dfMeadBathymetry$`Elevation (ft)` , y=dfMeadBathymetry$`Active Storage (ac-ft)`, method="linear") / 1e6
    dfPowellElevations$ActiveStorageMAF <- interp2(xi = dfPowellElevations$`Elevation (feet)`, x=dfPowellBathymetry$`ELEVATION (feet)` , y=dfPowellBathymetry$`Active Storage (acre-feet)`, method="linear") / 1e6
    
    #Combine Elevations and Labels
    dfMeadElevations$Label <- paste0(dfMeadElevations$`Elevation (feet)`," - ", dfMeadElevations$Description)
    dfPowellElevations$Label <- paste0(dfPowellElevations$`Elevation (feet)`," - ", dfPowellElevations$Description)

    return(list(dfMeadBathymtery = dfMeadBathymetry, dfPowellBathymetry = dfPowellBathymetry, dfMeadElevations = dfMeadElevations, dfPowellElevations = dfPowellElevations))
    }

#### Function: Read in ICS balances and calculate annual contributions

fReadICSData <- function() {
  ## Read in ICS account balance data
  sExcelFile <- 'IntentionallyCreatedSurplus-Summary.xlsx'
  sExcelFile <- here("AutoReadUSBRData", sExcelFile)
  
  dfICSBalance <- read_excel(sExcelFile, sheet = "Balances")

  #Save the most recent year of ICS data
  nMaxYearICSData <- max(dfICSBalance$Year)
  #Register the largest year of reservoir data. Right now one larger than ICS
  nMaxYearResData <- nMaxYearICSData + 1
  
  dfICSBalanceForStacked <- dfICSBalance

  # #Duplicate the largest year and set the year to largest value plus 1
   dfICSBalanceForStacked <- rbind(dfICSBalanceForStacked, dfICSBalanceForStacked %>% filter(Year == nMaxYearICSData) %>% mutate(Year = nMaxYearICSData+1))
  # #Order by decreasing year
  dfICSBalanceForStacked <- dfICSBalanceForStacked[order(-dfICSBalanceForStacked$Year),]
  #Turn time into a index by month. Year 1 = 1, Year 2 = 13
  dfICSBalanceForStacked$MonthIndex <- 12*(dfICSBalanceForStacked$Year - dfICSBalanceForStacked$Year[nrow(dfICSBalanceForStacked)]) + 12

  cColNames <- colnames(dfICSBalance)
  #Convert to Narrow data frame so state columns become a variable
  dfICSBalanceNarrow <- melt(data = dfICSBalance,id.vars = "Year", measure.vars = cColNames[2:5])
  
  ##Turn the ICS year into monthly
  dfICSmonths = expand.grid(Year = unique(dfICSBalanceForStacked$Year), month = 1:12)
  dfICSmonths$MonthIndex <- 12*(dfICSmonths$Year - dfICSmonths$Year[nrow(dfICSmonths)]) + dfICSmonths$month
  #Filter off first year but keep last month
  dfICSmonths <- dfICSmonths %>% filter(dfICSmonths$MonthIndex >= 12)
  #Calculate a date
  dfICSmonths$Date <- as.Date(sprintf("%d-%d-01",dfICSmonths$Year, dfICSmonths$month))
  
  #Interpolate Lower Basin conservation account balances by Month
  dfICSmonths$LowerBasinConserve <- interp1(xi = dfICSmonths$MonthIndex, x=dfICSBalanceForStacked$MonthIndex, y = dfICSBalanceForStacked$Total, method="linear" )
  #Interpolate Mexico conservation account balance by Month
  dfICSmonths$MexicoConserve <- interp1(xi = dfICSmonths$MonthIndex, x=dfICSBalanceForStacked$MonthIndex, y = dfICSBalanceForStacked$Mexico, method="linear" )
  
  ##Set values above the max ICS date to zero
  dfICSmonths[dfICSmonths$Year > nMaxYearICSData, c("LowerBasinConserve", "MexicoConserve")] <- 0


  
  
  
  
  ##Format Program limits for plotting
  dfLimits <- read_excel(sExcelFile, sheet = "Capacities",  range = "A7:F10")
  cColNamesLimits <- colnames(dfLimits)
  dfLimitsMelt <- melt(data=dfLimits, id.vars="New levels with DCP", measure.vars = cColNamesLimits[2:5]) 
  dfMaxBalanceCum = dfLimitsMelt %>% filter(`New levels with DCP` == "Max Balance (AF)", variable != 'Total')
  #Reorder so Arizona is on top
  dfMaxBalanceCum$Order <- c(3,2,1,4)
  dfMaxBalanceCum <- dfMaxBalanceCum[order(dfMaxBalanceCum$Order),]
  #Calculate the cumulative total
  dfMaxBalanceCum$CumVal <- cumsum(dfMaxBalanceCum$value)
  #Replace the Arizona label
  dfMaxBalanceCum$StateAsChar <- as.character(dfMaxBalanceCum$variable)
  dfMaxBalanceCum$StateAsChar[3] <- "Total/Arizona"
  
  dfMaxAnnualAmounts <- data.frame(Year=dfICSBalance$Year, MaxDeposit = dfLimits$Total[1], MaxWithdraw = dfLimits$Total[3])
  
  
  ## Calculate Credits/Debits from ICS balances as differences by year
  
  # Add a row of zeros year for the year before the first year
  cFirstICSYear <- min(dfICSBalance$Year)
  dfICSBalanceFirstYear <- dfICSBalance[1,]
  dfICSBalanceFirstYear[1, 2:(ncol(dfICSBalanceFirstYear))] <- 0
  dfICSBalanceFirstYear$Year <- cFirstICSYear - 1
  dfICSBalanceFirstYear$Item <- paste("Balance - Dec",as.character(cFirstICSYear - 1) ,"(AF)")
  
  dfICSBalanceAll <- rbind(dfICSBalanceFirstYear, dfICSBalance)
  
  dfICSDeposit <- data.frame(diff(as.matrix(dfICSBalanceAll %>% select(Arizona,California,Nevada,Mexico,Total,Year))))
  
  #dfICSDeposit <- data.frame(-diff(as.matrix(dfICSBalanceAll %>% select(Arizona,California,Nevada,Mexico,Total,Year))))
  #Put the correct year back in
  dfICSDeposit$Year <- dfICSBalance$Year[1:nrow(dfICSDeposit)]

  #Melt the data so state columns become a variable
  dfICSDepositNarrow <- melt(data = dfICSDeposit,id.vars = "Year", measure.vars = cColNames[2:5])
  
  return(list(dfICSBalance = dfICSBalance, dfICSBalanceNarrow = dfICSBalanceNarrow, dfICSmonths = dfICSmonths, dfMaxBalanceCum = dfMaxBalanceCum, dfICSLimits = dfLimits, dfICSDepositNarrow = dfICSDepositNarrow, dfMaxAnnualAmounts = dfMaxAnnualAmounts, nMaxYearICSData = nMaxYearICSData, nMaxYearResData = nMaxYearResData))
  }


#dfTemp <- ReadBathymetryCritialElevations()

# Test Trial runs
# Read the most recently saved csv files
# lResData <- fReadReclamationHydroData(FromHydroData = FALSE)
#
# Read the downloaded data from HydroData
# lResData <- fReadReclamationHydroData(FromHydroData = TRUE)

lTrialICS <- fReadICSData()
