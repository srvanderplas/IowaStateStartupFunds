# R libraries -------------------------------------------------------------------
library(plyr) # for rbind.fill function
library(dplyr) # for %>% operator
library(stringr) # for string manipulation
library(lubridate) # for date manipulation
library(reshape2) # for reshaping data
library(ggplot2) # for plotting
library(scales) # for greater legend/scale control
# devtools::install_github("srvanderplas/GoogleScholarScrapeR")
library(GoogleScholarScrapeR)
# -------------------------------------------------------------------------------

# Read in startup funding information
source("Code/CleanStartupFundingData.R")

hire.names <- hires$Name

citations <- hires[1:5,] %>% group_by(Name) %>% do({Sys.sleep(10); author_search(.$Name, "Iowa State University")})
