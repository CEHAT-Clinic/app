# init.R
#
# Example R code to install packages if not already installed
#
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(zoo)
library(gridExtra)
library(PurpleAirCEHAT)
library(markdown)
library(lubridate)
library(shinythemes)
library(testthat)
library(tryCatchLog)
library(futile.logger)
my_packages = c("ggplot2", "DT","plotly","markdown","lubridate","testthat","PurpleAirCEHAT", "zoo", "tryCatchLog", "futile.logger","gridExtra","shinyWidgets","tidyverse")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
