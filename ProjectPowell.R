here::i_am()

###Function that projects Lake Powell elevations/storage based on multiple factors
# that can be manipulated or left as default.
#Inputs: Inflow (MAF/yr), Release (MAF/yr), Additional Release(MAF/yr), Additional Release Duration (months), Duration (Months)
# Outputs: dataframe (storage_proj, elevation_proj, datetime)
# Default Values: Inflow = CRMMS Min Prob, Release = 6 MAF/yr, Additional Relese = 1, AR Duration = 12, Duration = 24 months

ProjectPowell <- function(Inflow = "CRMMS", Release = 6, Add_Release = 1, Add_Time = 12, Duration = 24)
{
  if Inflow == "CRMMS"{
    # Figure out how to integrate AutoReadCRMMS Function
    
  }
  else {
    
  }
}
# Defines Inflow and Release Proportion pattern
