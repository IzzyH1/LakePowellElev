# Interactive Lake Powell Calculation Tool
This code creates an interactive tool that allows users to input a release and additional inflow schedule. 
It then calculates the projected storage and pool elevation of Lake Powell based on the user inputs.
These projections are displayed on a graph with the current CRMMS 24 MS and user inputted key elevations.
The total inflow and releases from Lake Powell for the Water Years present in the projection are also calculated and displayed.

## Description of Contents

**PowellProjectionsInteractive.Rmd** - Creates the Shiny app and user interface for the interactive tool

**PowellElevProjections.Rmd** - Markdown document that uses the Lake Powell Calculation tool to look specifically at the affects of Flaming Gorge Additional Release strategies. Noninteractive

**HelperFunctions** - Folder that contains the data used in the project and 3 scripts sourced in the interactive tool
1. **AutoReadUSBRData** - Sources most recent data from Reclamation's Hydrodata Web Portal (https://www.usbr.gov/uc/water/hydrodata/reservoir_data/site_map.html). Also loads in bathymetry data for Lake Powell from the following table: https://doi.org/10.5066/P9O3IPG3
 Original script written by Dr. David Rosenberg https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/AutoReadUSBRData
3. **AutoReadCRMMS24MS.R** - Sources data from most recent CRMMS 24MS for Lake Powell and filters data by reservoir (Lake Powell or Lake Mead) and parameter (Storage, Inflow Volume, Release Volume, or Pool Elevation).
4. **ProjectPowell.R** - Script that performs all of the calculations necessary for the interactive tool and creates the final plot. Functions are as follows:

   *ProjectPowell:* Takes additional inflow and release schedule, hydrodata, and a base inflow scenario and creates a dataframe of monthly inflow and             outflow. This time series is then plugged into project_storage and a storage/elevation projection is returned.

   *project_storage:* Takes a dataframe with inflow, outflow, and dates and plugs it into a time series mass balance equation. It outputs a projection           dataframe with columns (datetime, storage, elevation, inflow, outflow, and label)

   *fplotprojection:* Creates a ggplot with projected storage and elevation

## Directions to Reproduce Results
### Software Needed
The software needed to reproduce these results are RStudio.
### Reproducibility
To reproduce the results for PowellProjectionsInteractive.Rmd, follow the directions below.

1. Install R.
- Search 'https://cran.r-project.org' into a search engine
- Click the download link at the top of the page that corresponds to your operating system (Linux, Windows, or MacOS)
- Go to the device's downloads and find the installer chosen in step 1b.
- Click on the installer and follow the directions.
2. Install RStudio
- Search 'https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads' in a search engine.
- Scroll to the section titled 'Direct Downloads (Open Source)'
- Click the download link that corresponds to your operating system.
- Go to the device's downloads and find the installer chosen in step 2c.
- Select the installer and follow the directions.
3. Download this repository.
- Scroll to the top of this page and select 'Code'.
- Select 'Download ZIP'.
- Go to the device's downloads and select 'LakePowellElev-main'. This will unzip the file.
5. Open the R Markdown script.
- In the 'LakePowellElev-main' file, select the file 'PowellProjectionsInteractive.Rmd'.
- Open 'PowellProjectionsInteractive.Rmd' with RStudio.
- At the top of the left panel is green arrow labeled 'Run'. Click this button
- The resulting shiny app will open as a separate window

## Contact Information
### Authors
#### Isabel I. Hinton. Email: isabel.hinton@usu.edu
#### David E. Rosenberg. Email: david.rosenberg@usu.edu

