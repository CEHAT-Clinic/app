# app repository
This repository contains all the code necessary to run the Interactive Data Analysis Shiny App. In order to access the information in the Shiny application, it is necessary to install RStudio as well as know how to run the interactive app. In the following sections we will discuss how you can this. 

# Software Installation

In order to run R and RStudio on your system, you need to follow the following three steps in order. Note that if you already have R installed, but it is a version that precedes version 4.0.5, you will need to update R (to check your version, type version into the command line and hit "Enter"). To update R, visit https://www.linkedin.com/pulse/3-methods-update-r-RStudio-windows-mac-woratana-ngarmtrakulchol/ first before moving onto the steps in Section "Installing Packages."

## Installing R

- Based on the operating system you are using, download the binary setup file in https://cran.r-project.org/ (for Windows) or the appropriate version of the .pkg file in https://cran.r-project.org/bin/macosx/ (for Mac).
- Open the downloaded file, and install R.

## Installing RStudio

Next, download RStudio desktop at https://www.RStudio.com/products/RStudio/download/}(the free, open source option), download the appropriate installer file for your operating system (Windows, macOS, Linux, etc.), and run it to install RStudio.

## Installing Packages

You can follow the instructions below to install packages.

- Run RStudio.
- Click on the Packages tab in the bottom-right section and then click on install. A dialog box will appear.
- Install the packages by copying and pasting the following into the "packages" line: automap devtools dplyr DT futile.logger gridExtra lubridate plotly shiny shinythemes shinyWidgets testthat tidyverse tryCatchLog zoo
- To install the PurpleAirCEHAT package, paste "devtools::install_github("CEHAT-Clinic/analysis")"  into the command line in the "Console" pane, and hit "Enter." If you are prompted to update 40 or so packages, press "Enter" again to skip this and continue.

This completes the installation procedure for R Studio.

# Running the Interactive Data Analysis App

- Clone the app repository from https://github.com/CEHAT-Clinic/app, following these instructions: https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository.
- Open "app.R" in RStudio. To do this, either double-click on app.R in its specified folder location (this will automatically open RStudio), or open RStudio and do the following:
    - File → Open File… → Locate app.R in its specified folder and click on it once → Open

- At this step, there may be some additional libraries that need to be installed. RStudio will inform you of this at the top of the document. Simply click on “Install” for each package that it requires. 
- Click "Run App" in the top right corner of the pane to run the application. 
- Upload data into the app to begin!


This completes the setup procedure for the Interactive Data Analysis App.
